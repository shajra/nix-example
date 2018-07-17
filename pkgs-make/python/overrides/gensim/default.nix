nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "gensim";
    version = "3.4.0";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1q6m5vpwix1i2aqn3wk0wkxb7n75algc6gzx32948xn1qy14r105";
    };

    propagatedBuildInputs = with self; [
        numpy
        scipy
        six
        smart_open
    ];

    checkInputs = with self; [ Keras tensorflow ];

    # DESIGN: tests require old version of tensorflow; doesn't seem worth the
    # effort to compile to pull in just for this test
    #
    doCheck = false;

}
