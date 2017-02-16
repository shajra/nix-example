{ stdenv
, writeReferencesToFile
}:

pkg:

stdenv.mkDerivation {

    name = pkg.name + ".tar.gz";

    buildInputs = [];

    deps = writeReferencesToFile pkg;
    input_ref = pkg;

    builder = ./builder.sh;

}
