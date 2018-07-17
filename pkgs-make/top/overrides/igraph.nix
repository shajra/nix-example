self: super:

super.stdenv.mkDerivation rec {

    pname = "igraph";
    version = "0.7.1";
    name = "${pname}-${version}";

    src = super.fetchurl {
        url = "http://igraph.org/nightly/get/c/${name}.tar.gz";
        sha256 = "1pxh8sdlirgvbvsw8v65h6prn7hlm45bfsl1yfcgd6rn4w706y6r";
    };

    buildInputs = with self; [ zlib libxml2 ];

    # IDEA: could patch source code for added security
    hardeningDisable = [ "format" ];

}
