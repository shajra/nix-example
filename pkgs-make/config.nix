let

    sources = (import ../support/nix).sources;

    sourceMetadata = builtins.fromJSON
        (builtins.readFile ../support/nix/sources.json);

    shajra-pkgs = import sources.shajra-nix-packages {};

    hls = ghcVersion:
        let hls' = import sources.nix-hls {inherit ghcVersion;};
        in [hls'.hls-renamed hls'.hls-wrapper];

    nixpkgs-stable = import sources.nixpkgs {
        config = {}; overlays = [];
    };

    justStatic = nixpkgs-stable.haskell.lib.justStaticExecutables;

    nixpkgs-unstable = import sources.nixpkgs-unstable {
        config = {}; overlays = [];
    };

    base = {

        nixpkgsRev = sourceMetadata.nixpkgs.rev;
        nixpkgsSha256 = sourceMetadata.nixpkgs.sha256;

        nixpkgsArgs.config = {
            allowUnfree = true;
            cudaSupport = true;
        };

        bootPkgsPath = sources.nixpkgs;
        bootPkgs = null;
        basePkgsPath = null;
        overlay = self: super: {};
        extraOverlay = self: super: {};
        srcFilter = lib: lib.nix.sources.ignoreDevCommon;
        extraSrcFilter = lib: p: t: true;
        srcTransform = lib: s: s;
        haskellArgs = {};
        pythonArgs = {};

    };

    haskell = {
        ghcVersion = "ghc884";
        overrides = pkgs: self: super: {};
        extraOverrides = pkgs: self: super: {};
        srcFilter = lib: lib.nix.sources.ignoreDevHaskell;
        extraSrcFilter = lib: p: t: true;
        srcTransform = lib: s: s;
        pkgChanges = lib: {};
        changePkgs = {};
        shajraEnvTools = {nixpkgs, ghcVersion}: hls ghcVersion ++ [
            (import sources.nix-haskell-tags).nix-haskell-tags-exe
            (shajra-pkgs.cabal2nix)
            (shajra-pkgs.cabal-install)
            (shajra-pkgs.ghcid)
            (shajra-pkgs.hlint)
            (shajra-pkgs.hoogle)
            (shajra-pkgs.refactor)
            (shajra-pkgs.stylish-haskell)
            (shajra-pkgs.gen-hie)
        ];
        envTools = {nixpkgs, ghcVersion}: [
            (justStatic nixpkgs-stable.haskellPackages.cabal2nix)
            (justStatic nixpkgs-stable.haskellPackages.cabal-install)
            (justStatic nixpkgs-stable.haskellPackages.ghcid)
            (justStatic nixpkgs-stable.haskellPackages.hlint)
            (justStatic nixpkgs-stable.haskellPackages.hoogle)
            (justStatic nixpkgs-stable.haskellPackages.stylish-haskell)
        ];
    };

    python = {
        pyVersion = "38";
        overrides = pkgs: self: super: {};
        extraOverrides = pkgs: self: super: {};
        srcFilter = lib: lib.nix.sources.ignoreDevPython;
        extraSrcFilter = lib: p: t: true;
        srcTransform = lib: s: s;
        envTools = {nixpkgs, pyVersion}: [
            nixpkgs.autoflake
            nixpkgs."python${pyVersion}Packages".flake8
            nixpkgs."python${pyVersion}Packages".ipython
            nixpkgs."python${pyVersion}Packages".isort
            nixpkgs."python${pyVersion}Packages".pylint
            nixpkgs."python${pyVersion}Packages".yapf
            nixpkgs-unstable.nodePackages.pyright
        ];
        envPersists = true;
    };

in

rec {

    plain = { inherit base haskell python; };

    curated = {
        base    = plain.base    // { overlay   = import top/overrides;    };
        haskell = plain.haskell // { overrides = import haskell/overrides; };
        python  = plain.python  // { overrides = import python/overrides;  };
    };

}
