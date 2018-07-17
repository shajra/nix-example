nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "mocket";
    version = "2.2.3";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "04hmksw1akm7q9rv506qvl5pxcw40qv7ggmnlz8hnsxpfzq3chwi";
    };

    propagatedBuildInputs = with self; [
        decorator
        hexdump
        python_magic
        six
    ];

    doCheck = false;

}
