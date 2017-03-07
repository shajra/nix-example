let

    default =
        {
            bootPkgs = import <nixpkgs> {};
            nixpkgsArgs = {};
            overlay = import ./overrides/nixpkgs;
            srcFilter = p: t:
                baseNameOf p != "result" && baseNameOf p != ".git";
            haskellArgs = {};
        };

in

{ rev
, sha256
, bootPkgs ? default.bootPkgs
, nixpkgsArgs ? default.nixpkgsArgs
, srcFilter ? default.srcFilter
, nixpkgsOverlay ? default.overlay
, haskellArgs ? default.haskellArgs
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

    morePkgs = self: super:
        let
            hArgs = { nixpkgs = self; inherit pkgs; } // haskellArgs;
            h = import ./haskell.nix hArgs;
            extnPkgs = import ./tools self.callPackage;
        in
        {
            haskellPackages = h.haskellPackages;
            pkgsMake = {
                args = {
                    call = {
                        haskell = {
                            lib = h.callHaskellLib;
                            app = h.callHaskellApp;
                        };
                    };
                };
                env = { haskell = h.env; };
            };
        } // extnPkgs // pkgs;

    overlays = [ nixpkgsOverlay morePkgs ];

    nixpkgs =
        import origNixpkgs.path
            (nixpkgsArgs // { inherit overlays; });

    callPackage = p:
        let pkg = (nixpkgs.callPackage (import p) {});
        in
        if pkg ? overrideAttrs
        then pkg.overrideAttrs (attrs:
            if attrs ? src
            then { src = builtins.filterSource srcFilter attrs.src; }
            else {})
        else pkg;

    generatorArgs = {
        lib = import ./lib nixpkgs;
        call = {
            package = callPackage;
            haskell = nixpkgs.pkgsMake.args.call.haskell;
        };
    };

    pkgs = generator generatorArgs;

in

pkgs // { inherit nixpkgs; env = nixpkgs.pkgsMake.env; }
