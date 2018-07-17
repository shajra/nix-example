{ stdenv
, makeWrapper
, haskell
, haskellPackages
, which
}:

stdenv.mkDerivation {

    name = "nix-tags-haskell";

    src = ./.;

    buildInputs = [ makeWrapper ];

    hasktags = haskellPackages.hasktags;
    haskdogs = haskellPackages.haskdogs;
    inherit which;

    builder = ./builder.sh;

}
