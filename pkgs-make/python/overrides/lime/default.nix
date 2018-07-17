nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "lime";
    version = "0.1.1.31";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1hcjqkq19a92mpv0g4qcik6ffaxi65ygmw491n2x64rnsimm86kh";
    };

    propagatedBuildInputs = with self; [
        numpy
        scipy
        scikitlearn
        scikitimage
    ];

    doCheck = false;

}
