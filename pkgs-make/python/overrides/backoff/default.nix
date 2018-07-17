nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "backoff";
    version = "1.5.0";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "01bqbb7bdkrh60lv5is597wjgd9fjkl42wig7yn71jpssy8j4h61";
    };

}
