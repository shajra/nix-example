nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "avro-python3";
    version = "1.8.2";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1mhvp3m9dq4wdyzxq8v7ay4w39mcsl56abs4dcg0nq49c7bg0b7q";
    };

    doCheck = false;

}
