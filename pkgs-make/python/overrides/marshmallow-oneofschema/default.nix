nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "marshmallow-oneofschema";
    version = "1.0.5";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0i6rbnclggzap1izxhna6wcc6npsbb79zbf6c8ikcmr6b35msr2v";
    };

    propagatedBuildInputs = with self; [ marshmallow ];

    doCheck = false;

}
