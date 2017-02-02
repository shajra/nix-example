let overrides = import ./overrides; in

{ rev
, sha256
, bootPkgs ? import <nixpkgs> {}
, nixpkgsOverride ? overrides.nixpkgs
, haskellPackagesOverride ? overrides.haskell
, srcFilter ? p: t:
    baseNameOf p != "result"
        && baseNameOf p != ".stack-work"
        && baseNameOf p != ".git"
}:
generator:

let

    nixpkgsPath =
        bootPkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            inherit rev sha256;
        };
    origNixpkgs = import nixpkgsPath {};
    nixpkgs = import origNixpkgs.path { overlays = [ nixpkgsOverride ]; };
    lib = { nix = nixpkgs.lib; haskell = nixpkgs.haskell.lib; };
    ghc = nixpkgs.haskell.packages.ghc801;
    haskellPackages =
        ghc.override { overrides = haskellPackagesOverride nixpkgs; };
    filterDrv = drv:
        if drv ? overrideAttrs
        then
            drv.overrideAttrs (attrs:
                if attrs ? src
                then { src = builtins.filterSource srcFilter attrs.src; }
                else {})
        else drv;
    callPackage = p:
        filterDrv
            (lib.nix.callPackageWith
                (haskellPackages // nixpkgs // pkgs) (import p) {});
    haskellFilterSrc = drv: lib.haskell.overrideCabal drv (attrs:
        { src = builtins.filterSource srcFilter attrs.src; });
    drvHaskell = p:
        if builtins.pathExists (builtins.toPath (p + "/default.nix"))
        then p
        else
            nixpkgs.stdenv.mkDerivation {
                name = "cabal2nix-autogen";
                buildInputs = [ nixpkgs.cabal2nix ];
                phases = [ "installPhase" ];
                src = builtins.filterSource srcFilter p;
                installPhase =
                    ''
                    mkdir "$out"
                    cabal2nix "$src" > "$out/default.nix"
                    '';
            };
    callHaskell = p:
        lib.haskell.dontHaddock
            (haskellFilterSrc
                (lib.nix.callPackageWith
                    (nixpkgs // haskellPackages // pkgs)
                    (import (drvHaskell p))
                        {}));
    callHaskellApp = p:
        lib.haskell.disableSharedExecutables (callHaskell p);
    makePkgs = {
        lib = lib;
        call = {
            package = callPackage;
            haskell = {
                lib = callHaskell;
                app = callHaskellApp;
            };
        };
    };
    pkgs = generator makePkgs;
    envKeys =
        [ "nativeBuildInputs"
            "propagatedNativeBuildInputs"
            "buildInputs"
            "propagatedBuildInputs" ];
    env =
        with lib.nix;
        let
            isLeaf = s: isDerivation s && ! s ? env;
            isDepKey = a: v: any (b: a == b) envKeys;
            getDeps = s: flatten (attrValues (filterAttrs isDepKey s));
            pruneAttrs = s:
                if isDerivation s
                then { env = s.env; }
                else filterAttrs (n: v: isAttrs v && ! v ? __functor) s;
            foldBranch = s:
                unique
                    (flatten
                        (attrValues (mapAttrs (n: foldSet) (pruneAttrs s))));
            foldSet = s:
                if isAttrs s
                then if isLeaf s then getDeps s else foldBranch s
                else [];
            deps = foldSet pkgs;
        in
            nixpkgs.stdenv.mkDerivation {
                name = "env-interactive";
                nativeBuildInputs = deps;
            };

in

    pkgs // { env = env; }
