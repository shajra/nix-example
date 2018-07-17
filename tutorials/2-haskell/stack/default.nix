# By specifying a custom Nix expression for Haskell Stack [1], we can use the
# same pinned Nixpkgs specified in the top-level build.nix, rather than having
# to respecify it in stack.yaml.
#
# Unfortunately, the dependencies pulled in by the resolver in stack.yaml can
# fall out of sync with those specified in our Nix build.
#
# https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file

{ pkgs  # our pinned Nixpkgs

# shared libraries needed for building
, gmp
, zlib

}:

{ghc}:

pkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "env-stack";
    buildInputs = [ gmp zlib ];
}
