let

    base = {

        # git describe: 18.09-beta-2447-g89b618771ad
        nixpkgsRev = "89b618771ad4b0cfdb874dee3d51eb267c4257dd";
        nixpkgsSha256 = "0jlyggy7pvqj2a6iyn44r7pscz9ixjb6fn6s4ssvahfywsncza6y";

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
        ghcVersion = "ghc843";
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
