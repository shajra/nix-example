callPackage:

{
    nix-tags-haskell = callPackage (import ./nix-tags-haskell) {};
    cabal-new-watch = callPackage (import ./cabal-new-watch) {};
}
