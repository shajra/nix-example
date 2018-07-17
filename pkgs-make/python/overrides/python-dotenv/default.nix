nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "python-dotenv";
    version = "0.8.2";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1mc901wfxd0sxw0baqcb08dh66waarkfcx4r009ic4apa8c3d5sh";
    };

    checkInputs = with self; [ click ipython ];

}
