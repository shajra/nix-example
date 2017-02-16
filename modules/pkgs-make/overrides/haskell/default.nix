pkgs: self: super:

{
    # DESIGN: no overrides needed any more, but here's the pattern.
    #
    # haskakafka = (import ./haskakafka.nix) pkgs self super;
}
