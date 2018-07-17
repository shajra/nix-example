self: super:

super.stdenv.mkDerivation rec {

    pname = "GeographicLib";
    version = "1.49";
    name = "${pname}-${version}";

    src = super.fetchurl {
        url = "https://downloads.sourceforge.net/project/"
            + "${super.lib.toLower pname}/distrib/${name}.tar.gz";
        sha256 = "0azf2lcg39lgrg1di52fwzb22zi5aapy6xqakmfl9jdrnr9aph5f";
    };

}
