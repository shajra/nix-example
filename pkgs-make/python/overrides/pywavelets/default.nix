nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "PyWavelets";
    version = "0.5.2";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1ndy2c0ihpnvwl1lclmc1rml8r0z7wv1b5dhj0a7i8cfckqf4dnf";
    };

    propagatedBuildInputs = with self; [ numpy ];

    checkInputs = with self; [ nose ];

}
