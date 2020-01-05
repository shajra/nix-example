let
    sources = import ./sources.nix;
    nix-project-all = import sources.nix-project;
    nixpkgs = import sources.nixpkgs {};
in
    { inherit nixpkgs; }
    // nix-project-all
