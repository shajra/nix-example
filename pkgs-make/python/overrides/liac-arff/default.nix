nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "liac-arff";
    version = "2.2.2";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0sbw2v1kibak66872xdc9hgbykz9gzw6w42zcr14qg27wxsh9dpl";
    };

    checkInputs = with self; [ mock ];

    doCheck = false;

}
