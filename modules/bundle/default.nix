{ stdenv
, ekg
, ekg-assets
, example-app
, replace
}:

let
    service = example-app;
    assets = ekg-assets;
in

stdenv.mkDerivation {
    name = "example-bundle";
    buildInputs = [ replace ];
    inherit service ekg assets;
    builder = ./builder.sh;
}
