nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "lda";
    version = "1.0.5";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "06vdq7lwmfz8iwmg4vh10l3q51ikvvfmzlqzhzbsr81knpii472f";
    };

    buildInputs = with self; [
        cython
        flake8
        sphinx_rtd_theme
        numpydoc
    ];

    propagatedBuildInputs = with self; [
        pbr
        numpy
    ];

    doCheck = false;

}
