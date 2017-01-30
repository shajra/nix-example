{ stdenv
, findutils
, ekg
}:

stdenv.mkDerivation {
    name = "ekg-assets";
    buildInputs = [ findutils ];
    inherit ekg;
    builder = ./builder.sh;
}
