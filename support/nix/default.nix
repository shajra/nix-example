let
    sources = import ./sources-unified.nix;
    nix-project-all = import sources.nix-project;
    nixpkgs = import sources.nixpkgs { config = {}; overlays = []; };
in
    { inherit nixpkgs sources; }
    // nix-project-all
