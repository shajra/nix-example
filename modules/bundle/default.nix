{ stdenv
, ekg-assets
, strings-replace
, example-app
}:

let
    service = example-app;
    assets = ekg-assets;
in

stdenv.mkDerivation {
    name = "example-bundle";
    buildInputs = [ strings-replace ];
    inherit service assets;
    builder = ./builder.sh;
}
