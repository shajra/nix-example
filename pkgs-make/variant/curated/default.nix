let
    variantMake = import ../variantMake.nix;
    defaults = (import ../../config.nix).curated;
in
    variantMake defaults