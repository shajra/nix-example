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
, envTools ? defaults.envTools { inherit nixpkgs ghcVersion; }
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

    callHaskellPath = forceCabal2nix: p:
        let
            parent = dirOf p;
            base = baseNameOf p;
            type = (builtins.readDir parent).${base} or null;
            isDir = type == "directory";
            isFile = type == "regular";
            name = lib.nix.composed [
                (lib.nix.removeSuffix ".cabal")
                (lib.nix.findFirst
                    (lib.nix.hasSuffix ".cabal")
                    ("unknown"))
                builtins.attrNames
                builtins.readDir
            ] (if isDir then p else parent);
            hasDefaultNix =
                builtins.pathExists (builtins.toPath (p + "/default.nix"));
        in
            if ! forceCabal2nix && (isFile || hasDefaultNix)
            then haskellPackages.callPackage (import p) {}
            else haskellPackages.callCabal2nix name p {};

    callHaskell = forceCabal2nix: x:
        if builtins.typeOf x == "path"
        then callHaskellPath forceCabal2nix x
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

    callHaskellLib = forceCabal2nix:
        lib.nix.composed [
            cleanSource
            tagPackages
            (callHaskell forceCabal2nix)
        ];

    callHaskellApp = forceCabal2nix: p:
        lib.haskell.justStaticExecutables (callHaskellLib forceCabal2nix p);

    envPkgs =
        builtins.filter
            (e: e._isLocalHaskellPackage or false)
            (builtins.attrValues pkgs);

    env =
        (haskellPackages.shellFor {
            packages = p: envPkgs;
            nativeBuildInputs = envTools;
        }).overrideAttrs (old1: {
            passthru.withEnvTools = f: env.overrideAttrs (old2: {
                nativeBuildInputs = f { inherit nixpkgs ghcVersion; };
            });
            passthru.withMoreEnvTools = f: env.overrideAttrs (old2: {
                nativeBuildInputs =
                    old2.nativeBuildInputs ++ f { inherit nixpkgs ghcVersion; };
            });
        });

in

{
    inherit
        pkgsChange
        haskellPackages
        env;
    call.lib = callHaskellLib false;
    call.app = callHaskellApp false;
    call.cabal2nix.lib = callHaskellLib true;
    call.cabal2nix.app = callHaskellApp true;
}
