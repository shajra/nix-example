{ stdenv
, coreutils
, fswatch
, gnugrep
, makeWrapper
}:

stdenv.mkDerivation {
    name = "cabal-new-watch";
    src = ./.;
    buildInputs = [ makeWrapper ];
    inherit coreutils fswatch gnugrep;
    builder = ./builder.sh;
}
