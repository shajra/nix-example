# Main build file for managing all modules

let

    # Version of Nixpkgs to lock down to for our build
    #
    pkgsMakeArgs = {
        # git describe: 16.09-beta-8886-g92b1e39e1c
        rev = "92b1e39e1c43a55a1460571782e3c7444556814b";
        sha256 = "1y62n4h70kh9a0pyigssfwfrdc1kfx9i4743722f2c6kf67j1v20";
    };

    # Library for making our packages (local copy)
    #
    pkgsMake = import modules/pkgs-make;

    # Alternatively, we can use the default Nixpkgs to pull a remote copy.
    # A remote copy allows us to share this Nix library with other projects.
    # Below we use `fetchFromGitHub`, but Nixpkgs has many other "fetch"
    # functions if you store your copy somewhere other than GitHub [FETCH].
    #
    # [FETCH] https://github.com/NixOS/nixpkgs/tree/master/pkgs/build-support
    #
    #pkgs-make =
    #    let
    #        pkgs-make-path =
    #            (import <nixpkgs> {}).fetchFromGitHub {
    #                owner = "shajra";
    #                repo = "example-nix";
    #                rev = "88d97891cba18f17770d7009c571bdf2ac58e39b";
    #                sha256 = "05zxagiagb0icr3f6l7y1camwjxj4w3kxhss06bwkinj8b4kq2k6";
    #            };
    #    in
    #    import (pkgs-make-path + "/modules/nix/call");

    # `pkgs-make` doesn't have a lot of code, but it does hide away enough
    # complexity to make this usage site simple and compact.
    #
    # If `pkgs-make` doesn't meet your all of your needs, you should be able
    # to modify it with some understanding of both Nix [NIX] and Nixpkgs
    # [NIXPKG], and the "call package" technique of calling functions in Nix
    # [CALLPKG].
    #
    # [NIX] http://nixos.org/nix/manual
    # [NIXPKGS] http://nixos.org/nixpkgs/manual
    # [CALLPKG] http://lethalman.blogspot.com/2014/09/nix-pill-13-callpackage-design-pattern.html
    #
    pkgs = pkgsMake pkgsMakeArgs ({ call, lib }:
        let
            modifiedHaskellCall = f:
                lib.nix.composed [
                    lib.haskell.enableLibraryProfiling
                    lib.haskell.doHaddock
                    f
                ];
            haskellLib = modifiedHaskellCall call.haskell.lib;
            haskellApp = modifiedHaskellCall call.haskell.app;
        in
        {
            ekg-assets = call.package modules/ekg-assets;
            example-lib = haskellLib modules/lib;
            example-app = haskellApp modules/app;
            example-bundle = call.package modules/bundle;
            example-tarball = lib.nix.tarball pkgs.example-bundle;
            stack = call.package modules/stack;
        });

in

pkgs
