nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "geographiclib";
    version = "1.49";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0cv514gcafbxpa3p6prz5aqxxxxc7l85v1r83fw5f2p8zi4acpb3";
    };

    buildInputs = with nixpkgs; [ geographiclib ];

}
