let

    base = {

        # git describe: 18.03-beta-12822-g5ac6ab091a4
        nixpkgsRev = "14a9ca27e69e33ac8ffb708de08883f8079f954a";
        nixpkgsSha256 = "1grsq8mcpl88v6kz8dp0vsybr0wzfg4pvhamj42dpd3vgr93l2ib";

        nixpkgsArgs.config = {
            allowUnfree = true;
            cudaSupport = true;
        };

        bootPkgsPath = <nixpkgs>;
        bootPkgs = null;
        basePkgsPath = null;
        overlay = self: super: {};
        extraOverlay = self: super: {};
        srcFilter = lib: lib.nix.sources.cleanCommonDev;
        extraSrcFilter = lib: p: t: true;
        haskellArgs = {};
        pythonArgs = {};

    };

    haskell = {
        ghcVersion = "ghc822";
        overrides = pkgs: self: super: {};
        extraOverrides = pkgs: self: super: {};
        srcFilter = lib: lib.nix.sources.cleanHaskell;
        extraSrcFilter = lib: p: t: true;
        pkgChanges = lib: {};
        changePkgs = {};
        envMoreTools = nixpkgs: [
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
        pyVersion = "36";
        overrides = pkgs: self: super: {};
        extraOverrides = pkgs: self: super: {};
        srcFilter = lib: lib.nix.sources.cleanPython;
        extraSrcFilter = lib: p: t: true;
        envMoreTools = nixpkgs: [
            nixpkgs.autoflake
            nixpkgs.pythonPackages.flake8
            nixpkgs.pythonPackages.ipython
            nixpkgs.pythonPackages.pylint
            nixpkgs.pythonPackages.yapf
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
