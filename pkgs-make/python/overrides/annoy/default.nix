nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "annoy";
    version = "1.12.0";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0w7clis5c6hhghqpw470jbk16fp8mdm88npf22g1xp3ig5psaalg";
    };

    checkInputs = with self; [ nose ];

}
