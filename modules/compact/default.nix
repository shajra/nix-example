{ stdenv
, ekg-assets
, example-app-static
, haskellPackages
, replace
}:

stdenv.mkDerivation {

    name = "example-compact";

    buildInputs = [ replace ];

    service = example-app-static;
    assets = ekg-assets;
    ekg = haskellPackages.ekg;

    builder = ./builder.sh;

}
