nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "en_core_web_md";
    version = "2.0.0";
    name = "${pname}-${version}";

    src = nixpkgs.fetchurl {
        url = "https://github.com/explosion/spacy-models/"
            + "releases/download/${name}/${name}.tar.gz";
        sha256 = "1b5g5gma1gzm8ffj0pgli1pllccx5jpjvb7a19n7c8bfswpifxzc";
    };

    propagatedBuildInputs = with self; [
        pathlib
        spacy
    ];

}
