nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "scikit-image";
    version = "0.14.0";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "090jvvjqj88f1n6p2vvcbgvak71h8ls34zz36q8kgigvh3mpaprj";
    };

    buildInputs = with self; [ cython ];

    propagatedBuildInputs = with self; [
        cloudpickle
        dask
        matplotlib
        networkx
        numpy
        pillow
        pywavelets
        scipy
        six
    ];

    # DESIGN: tests fail because the loader cannot create test objects
    doCheck = false;

}
