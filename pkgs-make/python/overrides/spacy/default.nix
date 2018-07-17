nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "spacy";
    version = "2.0.11";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "08d4bf9r8gykl921l2lbxwcspbcdvdc257dmkqry88jjjvkhdnyd";
    };

    # DESIGN: regex upgrade to what's in Nixpkgs is probably fine
    prePatch = ''
        substituteInPlace setup.py \
            --replace "regex==2017.4.5" "regex>=2017.4.5"
    '';

    buildInputs = with self; [ cython pip ];

    propagatedBuildInputs = with self; [
        cymem
        dill
        murmurhash
        numpy
        pathlib
        plac
        preshed
        regex
        thinc
        ujson
    ];

    checkInputs = with self; [ mock pytest ];

    doCheck = false;

}
