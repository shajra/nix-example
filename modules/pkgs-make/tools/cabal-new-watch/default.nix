{ stdenv
, makeWrapper
, gnugrep
, inotify-tools
}:

stdenv.mkDerivation {
    name = "cabal-new-watch";
    src = ./.;
    buildInputs = [ makeWrapper ];
    inherit gnugrep;
    inotifyTools = inotify-tools;
    builder = ./builder.sh;
}
