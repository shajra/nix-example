nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "psycopg2";
    version = "2.7.5";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "17klx964gw8z0znl0raz3by8vdc7cq5gxj4pdcrfcina84nrdkzc";
    };

    buildInputs = [ nixpkgs.postgresql ];

    doCheck = false;

}
