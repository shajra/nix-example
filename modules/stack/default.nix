{ pkgs }:
{ghc}:
pkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "env-stack";
    buildInputs = [ ];
}
