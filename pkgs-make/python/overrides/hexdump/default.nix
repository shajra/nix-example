nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "hexdump";
    version = "3.3";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1nr0dy3m8grdmd2hp3ym6x8vv9yki8zfgbba6vwy7b0n1hxs90fp";
        extension = "zip";
    };

    preConfigure = ''
        cd ..
    '';

}
