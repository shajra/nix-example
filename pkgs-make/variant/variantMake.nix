defaults:

{ nixpkgsRev ? defaults.base.nixpkgsRev
, nixpkgsSha256 ? defaults.base.nixpkgsSha256
, bootPkgsPath ? defaults.base.bootPkgsPath
, bootPkgs ? defaults.base.bootPkgs
, basePkgsPath ? defaults.base.basePkgsPath
, nixpkgsArgs ? defaults.base.nixpkgsArgs
, srcFilter ? defaults.base.srcFilter
, extraSrcFilter ? defaults.base.extraSrcFilter
, overlay ? defaults.base.overlay
, extraOverlay ? defaults.base.extraOverlay
, haskellArgs ? defaults.base.haskellArgs
, pythonArgs ? defaults.base.pythonArgs
}:

generator:

let

    chosenBootPkgs =
        if isNull bootPkgs
        then import bootPkgsPath {}
        else bootPkgs;

    nixpkgsPath =
        if isNull basePkgsPath
        then
            chosenBootPkgs.fetchFromGitHub {
                owner = "NixOS";
                repo = "nixpkgs";
                rev = nixpkgsRev;
                sha256 = nixpkgsSha256;
            }
        else basePkgsPath;

    nixpkgs = import nixpkgsPath (nixpkgsArgs // { inherit overlays; });

    overlays =
        (nixpkgsArgs.overlays or []) ++ [ overlay extraOverlay morePkgs ];

    morePkgs = self: super:
        let
            commonArgs = { nixpkgs = self; inherit pkgs; };
            hs = import ../haskell defaults.haskell (haskellArgs // commonArgs);
            py = import ../python defaults.python (pythonArgs // commonArgs);
            lib = import ../lib super;
            tools = import ../tools.nix super.callPackage;
            cleanSource = src:
                if lib.nix.sources.sourceLocal src
                then
                    lib.nix.sources.cleanSourceWith {
                        filter = p: t:
                            (srcFilter lib p t)
                            && (extraSrcFilter lib p t);
                        inherit src;
                    }
                else src;
            cleanSourceOverride = attrs:
                if attrs ? src then { src = cleanSource attrs.src; } else {};
            callPackage = p:
                let
                    expr = if builtins.typeOf p == "path" then import p else p;
                    pkg = super.callPackage expr {};
                in
                    if pkg ? overrideAttrs
                    then pkg.overrideAttrs cleanSourceOverride
                    else pkg;
        in
            {
                lib = lib.nix;
                haskell = super.haskell // { lib = lib.haskell; };
                haskellPackages = hs.haskellPackages;
                pythonPackages = py.pythonPackages;
                pkgsMake = {
                    inherit lib;
                    pkgsChange = hs.pkgsChange;
                    call = {
                        package = callPackage;
                        haskell = {
                            lib = hs.callHaskellLib;
                            app = hs.callHaskellApp;
                            hackage = hs.haskellPackages.callHackage;
                        };
                        python = py.callPython;
                    };
                    env = { haskell = hs.env; python = py.env; };
                };
            } // tools // pkgs;

    args = {
        lib = nixpkgs.pkgsMake.lib;
        call = nixpkgs.pkgsMake.call;
    };

    pkgs = generator args;

in

(nixpkgs.pkgsMake.pkgsChange pkgs) // {
    inherit nixpkgs;
    env = nixpkgs.pkgsMake.env;
}