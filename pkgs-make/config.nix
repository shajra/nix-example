let

    base = {

        # git describe: 18.09-beta-4191-g8070a6333f3
        nixpkgsRev = "895874d2145862249df3f78335f4dcf62ef01626";
        nixpkgsSha256 = "1mgagsaiky1in89vqg93brypm8n5g6ynrwi0imzlkjy5vpnq0995";

        nixpkgsArgs.config = {
            allowUnfree = true;
            cudaSupport = true;
        };

        bootPkgsPath = <nixpkgs>;
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
        ghcVersion = "ghc865";
        overrides = pkgs: self: super: {};
        extraOverrides = pkgs: self: super: {};
        srcFilter = lib: lib.nix.sources.ignoreDevHaskell;
        extraSrcFilter = lib: p: t: true;
        srcTransform = lib: s: s;
        pkgChanges = lib: {};
        changePkgs = {};
        envTools = {nixpkgs, ghcVersion}: [
            (nixpkgs.callPackage (import haskell/tools/nix-tags-haskell) {})
            (nixpkgs.callPackage (import haskell/tools/cabal-new-watch) {})
            nixpkgs.haskellPackages.apply-refact
            nixpkgs.haskellPackages.cabal2nix
            nixpkgs.haskellPackages.cabal-install
            nixpkgs.haskellPackages.ghcid
            nixpkgs.haskellPackages.hlint
            nixpkgs.haskellPackages.hoogle
            nixpkgs.haskellPackages.stylish-haskell
        ];
    };

    python = {
        pyVersion = "37";
        overrides = pkgs: self: super: {};
        extraOverrides = pkgs: self: super: {};
        srcFilter = lib: lib.nix.sources.ignoreDevPython;
        extraSrcFilter = lib: p: t: true;
        srcTransform = lib: s: s;
        envTools = {nixpkgs, pyVersion}: [
            nixpkgs.autoflake
            nixpkgs."python${pyVersion}Packages".flake8
            nixpkgs."python${pyVersion}Packages".ipython
            nixpkgs."python${pyVersion}Packages".pylint
            nixpkgs."python${pyVersion}Packages".yapf
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
