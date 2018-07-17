self: super:

rec {
    geographiclib = (import ./geographiclib.nix) self super;
    igraph = (import ./igraph.nix) self super;
}
