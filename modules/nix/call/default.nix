let overrides = import ./overrides; in

{ rev
, sha256
, nixpkgsOverride ? overrides.nixpkgs
, haskellPackagesOverride ? overrides.haskell
, srcFilter ? p: t:
    baseNameOf p != "result"
        && baseNameOf p != ".stack-work"
        && baseNameOf p != ".git"
}:
generator:

let

    bootPkgs = import <nixpkgs> {};
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
    generatorArgs = {
        lib = lib;
        call = {
            package = callPackage;
            haskell = {
                lib = callHaskell;
                app = callHaskellApp;
            };
        };
    };
    pkgs = generator generatorArgs;
    envDrv =
        with lib.nix;
        let
            envKeys = [ "nativeBuildInputs" "propagatedNativeBuildInputs" ];
            isEnvKey = a: v: any (b: a == b) envKeys;
            notInPkgs = d: ! elem d (attrValues pkgs);
            foundKeys =
                foldAttrs (a: acc: unique ((filter notInPkgs a) ++ acc)) []
                    (attrValues (mapAttrs (n: filterAttrs isEnvKey) pkgs));
        in
            ghc.mkDerivation {
                pname = "env-haskell";
                version = "0.1.0.0";
                src = ./.;
                libraryHaskellDepends =
                    if foundKeys ? propagatedNativeBuildInputs
                    then foundKeys.propagatedNativeBuildInputs
                    else [];
                libraryToolDepends =
                    [ haskellPackages.cabal-install haskellPackages.ghcid ] ++
                        (if foundKeys ? nativeBuildInputs
                        then foundKeys.nativeBuildInputs
                        else []);
                license = nixpkgs.stdenv.lib.licenses.bsd3;
            };

in

    pkgs // { env = envDrv.env; }
