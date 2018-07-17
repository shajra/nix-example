let
    variantMake = import ../variantMake.nix;
    defaults = (import ../../config.nix).plain;
in
    variantMake defaults