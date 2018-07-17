nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "PyJWT";
    version = "1.6.4";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0xi1nijkswi2sxq6448nhy3jxrw8mjppfibh8kxx6gymayri7r2f";
    };

    propagatedBuildInputs = with self; [
        cryptography
        ecdsa
    ];

    checkInputs = with self; [
        pytestrunner
        pytestcov
        pytest
        coverage
    ];

}
