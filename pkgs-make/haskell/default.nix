defaults:

{ nixpkgs
, pkgs
, ghcVersion ? defaults.ghcVersion
, pkgChanges ? defaults.pkgChanges
, changePkgs ? defaults.changePkgs
, overrides ? defaults.overrides
, extraOverrides ? defaults.extraOverrides
, srcFilter ? defaults.srcFilter
, extraSrcFilter ? defaults.extraSrcFilter
, srcTransform ? defaults.srcTransform
, envMoreTools ? defaults.envMoreTools nixpkgs
}:

let

    lib = import ../lib nixpkgs;

    rawHsPkgs = builtins.getAttr ghcVersion nixpkgs.haskell.packages;

    pkgChangesOverrides = self: super:
        lib.nix.mapAttrs (name: changes:
            lib.nix.composed changes super.${name}
        ) (pkgChanges lib);

    changePkgsOverrides =
       lib.nix.mapAttrsToList (c: ps: self: super:
           let
               toChange = p:
                   if builtins.hasAttr p super
                   then { ${p} = lib.haskell.${c} super.${p}; }
                   else {};
               changes = map toChange ps;
           in lib.nix.foldr (a: b: a // b) {} changes
       ) changePkgs;

    invertedChangePkgs =
        let
            inversions = lib.nix.mapAttrsToList (c: ps:
                let changeList = map (p: { ${p} = lib.haskell.${c}; }) ps;
                in lib.nix.foldAttrs (x: a: [x] ++ a) [] changeList
            ) changePkgs;
        in lib.nix.foldAttrs (x: y: x ++ y) [] inversions;

    pkgsChange = lib.nix.mapAttrs (attr: p:
        let changes1 = (pkgChanges lib).${attr} or [];
            changes2 = invertedChangePkgs.${attr} or [];
            changes = changes1 ++ changes2;
        in lib.nix.composed changes p
    );

    finalOverrides =
        let all = changePkgsOverrides ++ [
            pkgChangesOverrides
            (self: super: pkgs)
            (extraOverrides nixpkgs)
        ];
        in lib.nix.foldr lib.nix.composeExtensions (overrides nixpkgs) all;

    haskellPackages = rawHsPkgs.override { overrides = finalOverrides; };

    callHaskellPath = p:
        if builtins.pathExists (builtins.toPath (p + "/default.nix"))
        then haskellPackages.callPackage (import p) {}
        else
            let
                parent = dirOf p;
                base = baseNameOf p;
                type = (builtins.readDir parent).${base} or null;
                isDir = type == "directory";
                name = lib.nix.composed [
                    (lib.nix.removeSuffix ".cabal")
                    (lib.nix.findFirst
                        (lib.nix.hasSuffix ".cabal")
                        ("unknown"))
                    builtins.attrNames
                    builtins.readDir
                ] (if isDir then p else parent);
            in
            haskellPackages.callCabal2nix name p {};

    callHaskell = x:
        if builtins.typeOf x == "path"
        then callHaskellPath x
        else haskellPackages.callPackage x {};

    tagPackages = p:
        lib.haskell.overrideCabal p (old: {
            passthru = old.passthru or {} // {
                _isLocalHaskellPackage =
                    lib.nix.sources.sourceLocal old.src;
            };
        });

    filterSource = lib.nix.sources.filterSource
        (lib.nix.sources.allFilters [
            (extraSrcFilter lib)
            (srcFilter lib)
        ]);

    cleanSource = lib.haskell.transformSourceIfLocal
        (lib.nix.composed [ (srcTransform lib) filterSource ]);

    callHaskellLib = lib.nix.composed [
        cleanSource
        tagPackages
        callHaskell
    ];

    callHaskellApp = p: lib.haskell.justStaticExecutables (callHaskellLib p);

    envPkgs =
        builtins.filter
            (e: e._isLocalHaskellPackage or false)
            (builtins.attrValues pkgs);

    env =
        (haskellPackages.shellFor {
            packages = p: envPkgs;
            nativeBuildInputs = envMoreTools;
        }).overrideAttrs (old1: {
            passthru.withEnvTools = f: env.overrideAttrs (old2: {
                nativeBuildInputs =
                    old2.nativeBuildInputs ++ f nixpkgs;
            });
        });

in

{
    inherit
        callHaskellApp
        callHaskellLib
        pkgsChange
        haskellPackages
        env;
}
