{ pkgs
, gmp
, zlib
}:

{ghc}:

pkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "env-stack";
    buildInputs = [ gmp zlib ];
}