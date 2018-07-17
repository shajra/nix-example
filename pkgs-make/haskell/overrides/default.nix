pkgs: self: super:

{

    # DESIGN: https://github.com/NixOS/nixpkgs/issues/41079
    hasktags = pkgs.haskell.lib.dontCheck super.hasktags;

    # DESIGN: just an example of using callHackage
    #protolude = super.callHackage "protolude" "0.2" {};

}
