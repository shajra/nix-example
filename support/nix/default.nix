let
    sources = import ./sources.nix;
    nix-project-all = import sources.nix-project;
    nixpkgs = import sources.nixpkgs { config = {}; overlays = []; };
in
    { inherit nixpkgs sources; }
    // nix-project-all
