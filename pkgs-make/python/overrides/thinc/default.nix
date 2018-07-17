nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "thinc";
    version = "6.10.2";

    src = super.fetchPypi {
      inherit pname version;
      sha256 = "0xia81wvfrhyriywab184s49g8rpl42vcf5fy3x6xxw50a2yn7cs";
    };

    prePatch = ''
        substituteInPlace setup.py --replace \
            "'pathlib>=1.0.0,<2.0.0'," \
            "\"pathlib>=1.0.0,<2.0.0; python_version<'3.4'\","

        substituteInPlace setup.py --replace \
            "'cytoolz>=0.8,<0.9'," \
            "'cytoolz>=0.8',"

        substituteInPlace setup.py --replace \
            "'msgpack-numpy==0.4.1'" \
            "'msgpack-numpy'"
    '';

    buildInputs = with self; [ cython ];

    propagatedBuildInputs = with self; [
        cymem
        cytoolz
        dill
        msgpack-numpy
        msgpack-python
        murmurhash
        numpy
        plac
        preshed
        six
        termcolor
        tqdm
        wrapt
    ];

    checkInputs = with self; [
        hypothesis
        mock
        pytest
    ];

    # DESIGN: Nixpkgs doesn't run tests
    doCheck = false;

}
