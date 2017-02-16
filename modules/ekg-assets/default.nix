{ stdenv
, findutils
, haskellPackages
}:

stdenv.mkDerivation {
    name = "ekg-assets";
    buildInputs = [ findutils ];
    ekg = haskellPackages.ekg;
    builder = ./builder.sh;
}
