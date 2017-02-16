{ stdenv
, ekg-assets
, example-app
, haskellPackages
, replace
}:

stdenv.mkDerivation {

    name = "example-bundle";

    buildInputs = [ replace ];

    service = example-app;
    assets = ekg-assets;
    ekg = haskellPackages.ekg;

    builder = ./builder.sh;

}
