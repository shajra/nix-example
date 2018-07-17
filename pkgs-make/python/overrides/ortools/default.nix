nixpkgs: self: super:

let

    cbc = nixpkgs.cbc.overrideAttrs (oldAttrs: {
        name = "cbc-2.9.9";
        src = nixpkgs.fetchurl {
            url = "http://www.coin-or.org/download/source/Cbc/Cbc-2.9.9.tgz";
            sha256 = "1w8axdzm05xf5y13c31w7rc5z6ywxqxiwafnxcq3p195kgj0915a";
        };
        configureFlags = [ oldAttrs.configureFlags ] ++ [
            "--enable-static"
            "--disable-bzlib"
            "--without-lapack"
        ];
        dontDisableStatic = true;
    });

    glog = nixpkgs.glog.overrideAttrs (oldAttrs: rec {
        name = "glog-${version}";
        version = "0.3.5";
        src = nixpkgs.fetchFromGitHub {
          owner = "Google";
          repo = "glog";
          rev = "v${version}";
          sha256 = "12v7j6xy0ghya6a0f6ciy4fnbdc486vml2g07j9zm8y5xc6vx3pq";
        };
        dontDisableStatic = true;
    });

    protobuf3_3 = nixpkgs.protobuf.overrideDerivation (oldAttrs: rec {
        name = "protobuf-${version}";
        version = "3.3.0";
        src = nixpkgs.fetchFromGitHub {
          owner = "google";
          repo = "protobuf";
          rev = "v${version}";
          sha256 = "1258yz9flyyaswh3izv227kwnhwcxn4nwavdz9iznqmh24qmi59w";
        };
        dontDisableStatic = true;
    });

    python-protobuf = super.protobuf.override {
        protobuf = protobuf3_3;
    };

    deps = [
        cbc
        glog
        protobuf3_3
        nixpkgs.google-gflags
        nixpkgs.patchelf
    ];

    deps-env = nixpkgs.buildEnv {
        name = "ortools-dependencies";
        paths = deps;
    };

in

super.buildPythonPackage rec {

    version = "6.3";
    name = "ortools-${version}";

    src = nixpkgs.fetchurl {
        url = "https://github.com/google/or-tools/archive/v${version}.tar.gz";
        sha256 = "0ixka2ld700wnmp5rpgyc046q8wqgjs8g7liaxnsf80qkyixj3zn";
    };

    buildInputs = deps ++ [
        nixpkgs.zlib
        nixpkgs.swig
        nixpkgs.which
        nixpkgs.zlib
        super.python
    ];

    propagatedBuildInputs = [
        python-protobuf
    ];

    preConfigure = ''
        ln -s ${deps-env} dependencies/install
        find . \( -name '*.h' -o -name '*.cc' \) \
            -exec sed -i \
                -e '
                    s/<math\.h>/<cmath>/
                    s/<stdlib\.h>/<cstdlib>/
                ' {} \;
        sed -i \
            -e '
                s/install_python_modules pyinit/pyinit/
                s/$(PATCHELF)//
            ' makefiles/Makefile.python.mk
        cat /dev/null > tools/fix_libraries_on_linux.sh
        cat /dev/null > tools/fix_python_libraries_on_linux.sh
    '';

    preBuild = ''
        make missing_directories python pypi_archive
        rm -rf ortools
        cp -r temp-python*/ortools/* .
        find "$PWD" -name '*.so' | {
            while read -r f
            do
                patchelf \
                    --set-rpath "$(
                        patchelf --print-rpath "$f" \
                        | sed -e "s|$PWD[^:]*|\$ORIGIN/../../ortools|")" \
                    "$f"
            done
        }
    '';

}
