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

    ignoreMisc = p: t:
        let b = baseNameOf p;
        in (b != "cscope.out") && (b != ".direnv") && (b != ".envrc");

    ignoreEmacs = p: t:
        let b = baseNameOf p;
        in (! (nixpkgs.lib.hasSuffix "#" b && nixpkgs.lib.hasPrefix "#" b))
            && (! nixpkgs.lib.hasPrefix ".#" b)
            && (b != ".dir-locals.el")
            && (b != ".projectile");

    ignoreVim = p: t:
        let b = baseNameOf p;
        in (! nixpkgs.lib.hasSuffix "~" b)
            && (! nixpkgs.lib.hasSuffix ".swp" b)
            && (! nixpkgs.lib.hasSuffix ".swo" b);

    ignoreIdea = p: t:
        let b = baseNameOf p;
        in (b != ".idea") && (b != "idea_modules")
            && (! nixpkgs.lib.hasSuffix ".iml" b)
            && (! nixpkgs.lib.hasSuffix ".ipr" b)
            && (! nixpkgs.lib.hasSuffix ".iws" b);

    ignoreDevCommon = p: t:
        nixpkgs.lib.sources.cleanSourceFilter p t
            && ignoreEmacs p t
            && ignoreIdea p t
            && ignoreMisc p t
            && ignoreVim p t
            && ignoreTagFiles p t;

    ignoreDevPython = p: t:
        let b = baseNameOf p;
        in ignoreDevCommon p t
            && (b != "__pycache__")
            && (b != ".ipynb_checkpoints")
            && (! nixpkgs.lib.hasSuffix ".egg-info" b)
            && (! nixpkgs.lib.hasSuffix ".pyc" b);

    ignoreDevHaskell = p: t:
        let b = baseNameOf p;
        in ignoreDevCommon p t
            && (b != "dist-newstyle")
            && (b != "dist")
            && (b != ".stack-work")
            && (b != "cabal.project.local")
            && (b != ".cabal-sandbox")
            && (b != "cabal.sandbox.config")
            && (! nixpkgs.lib.hasPrefix ".ghc.environment." b);

    ignoreNixFiles = p: t: ! nixpkgs.lib.hasSuffix ".nix" (baseNameOf p);

    ignoreTagFiles = p: t:
        let b = baseNameOf p;
        in (b != "GPATH") && (b != "GRTAGS") && (b != "GTAGS")
            && (b != "tags") && (b != "TAGS");

    allFilters = builtins.foldl' (a: acc: p: t: a p t && acc p t) (p: t: true);

    transformSourceIf = cond: f: src:
        if cond src then f src else src;

    transformSourceIfLocal = transformSourceIf sourceLocal;

    filterSourceIf = cond: filter:
        transformSourceIf cond (src:
            nixpkgs.lib.sources.cleanSourceWith {
                inherit filter src;
            });

    filterSource = filterSourceIf (x: true);

    filterByRegex = regexes: src:
        nixpkgs.lib.sources.sourceByRegex src regexes;

    filterFilesBySuffices = exts: src:
        nixpkgs.lib.sources.sourceFilesBySuffices src exts;

    filterSourceIfLocal = filterSourceIf sourceLocal;

    sources = { inherit
        allFilters
        filterByRegex
        filterFilesBySuffices
        filterSource
        filterSourceIf
        filterSourceIfLocal
        ignoreDevCommon
        ignoreDevHaskell
        ignoreDevPython
        ignoreEmacs
        ignoreIdea
        ignoreMisc
        ignoreNixFiles
        ignoreTagFiles
        ignoreVim
        sourceLocal
        transformSourceIf
        transformSourceIfLocal
        unfilteredSource;
    };

    libExtn = sources // {
        inherit composed applying sources;
        dockerTools = nixpkgs.dockerTools;
        tarball = nixpkgs.callPackage ./tarball {};
        license-report = nixpkgs.callPackage ./license-report {} nixpkgs;
    };

in

    {
        nix = nixpkgs.lib.recursiveUpdate nixpkgs.lib libExtn;
        haskell = import ./haskell.nix nixpkgs;
    }
