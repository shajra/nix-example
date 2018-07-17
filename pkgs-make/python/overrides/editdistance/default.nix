nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "editdistance";
    version = "0.4";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1hdqxrrfl7d1pkqgrdkpjd9jr2rkmfwy82x5whi8klqpi1pxnrf7";
    };

}
