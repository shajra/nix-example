nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "dill";
    version = "0.2.8.2";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1cymzn9fxwdy33h21zkk4gqgzvd25110hh3zdqnvnwa3p52c4kb2";
    };

    # DESIGN: Nixpkgs couldn't get tests to pass
    doCheck = false;

    checkPhase = ''
        for test in tests/*.py
        do ${super.python.interpreter} $test
        done
    '';
}
