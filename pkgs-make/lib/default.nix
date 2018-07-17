nixpkgs:

let

    composed = builtins.foldl' (a: acc: b: a (acc b)) (a: a);

    applying = composed [ composed nixpkgs.lib.reverseList ];

    unfilteredSource = src:
        let ifFiltered = src ? _isLibCleanSourceWith;
        in if ifFiltered then src.origSrc else src;

    sourceLocal = src:
        ! nixpkgs.lib.hasPrefix builtins.storeDir
            (toString (unfilteredSource src));

    cleanMiscDev = p: t:
        let b = baseNameOf p;
        in (b != "cscope.out") && (b != ".direnv") && (b != ".envrc");

    cleanTagFiles = p: t:
        let b = baseNameOf p;
        in (b != "GPATH") && (b != "GRTAGS") && (b != "GTAGS")
            && (b != "tags") && (b != "TAGS");

    cleanEmacs = p: t:
        let b = baseNameOf p;
        in (! (nixpkgs.lib.hasSuffix "#" b && nixpkgs.lib.hasPrefix "#" b))
            && (! nixpkgs.lib.hasPrefix ".#" b)
            && (b != ".dir-locals.el")
            && (b != ".projectile");

    cleanVim = p: t:
        let b = baseNameOf p;
        in (! nixpkgs.lib.hasSuffix "~" b)
            && (! nixpkgs.lib.hasSuffix ".swp" b)
            && (! nixpkgs.lib.hasSuffix ".swo" b);

    cleanIdea = p: t:
        let b = baseNameOf p;
        in (b != ".idea") && (b != "idea_modules")
            && (! nixpkgs.lib.hasSuffix ".iml" b)
            && (! nixpkgs.lib.hasSuffix ".ipr" b)
            && (! nixpkgs.lib.hasSuffix ".iws" b);

    cleanCommonDev = p: t:
        nixpkgs.lib.sources.cleanSourceFilter p t
            && cleanEmacs p t
            && cleanIdea p t
            && cleanMiscDev p t
            && cleanTagFiles p t
            && cleanVim p t;

    cleanPython = p: t:
        let b = baseNameOf p;
        in cleanCommonDev p t
            && (b != "__pycache__")
            && (b != ".ipynb_checkpoints")
            && (! nixpkgs.lib.hasSuffix ".egg-info" b)
            && (! nixpkgs.lib.hasSuffix ".pyc" b);

    cleanHaskell = p: t:
        let b = baseNameOf p;
        in cleanCommonDev p t
            && (b != "dist-newstyle")
            && (b != "dist")
            && (b != ".stack-work")
            && (b != "cabal.project.local")
            && (b != ".cabal-sandbox")
            && (b != "cabal.sandbox.config")
            && (! nixpkgs.lib.hasPrefix ".ghc.environment." b);

    libExtn = {
        inherit composed applying;
        sources = { inherit
            cleanCommonDev
            cleanEmacs
            cleanHaskell
            cleanIdea
            cleanMiscDev
            cleanPython
            cleanTagFiles
            cleanVim
            sourceLocal
            unfilteredSource;
        };
        dockerTools = nixpkgs.dockerTools;
        tarball = nixpkgs.callPackage ./tarball {};
        license-report = nixpkgs.callPackage ./license-report {} nixpkgs;
    };

in

    {
        nix = nixpkgs.lib.recursiveUpdate nixpkgs.lib libExtn;
        haskell = import ./haskell.nix nixpkgs;
    }
