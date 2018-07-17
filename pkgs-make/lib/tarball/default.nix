{ stdenv
, writeReferencesToFile
}:

pkg: name:

stdenv.mkDerivation {

    inherit name;

    buildInputs = [];

    deps = writeReferencesToFile pkg;
    input_ref = pkg;

    builder = ./builder.sh;

}
