# Main build file for managing all modules

let

    # Version of Nixpkgs to lock down to for our build
    #
    pkgsMakeArgs = {
        # git describe: 16.09-beta-11812-gfa03b8279f
        rev = "fa03b8279fa9b544c29c97eaa5263163b6716046";
        sha256 = "1n8mwwg14xhm4arxafzfmf0wbr8smkgdvaagirxpni77jci81ar3";
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
    #pkgsMake =
    #    let
    #        pkgs-make-path =
    #            (import <nixpkgs> {}).fetchFromGitHub {
    #                owner = "shajra";
    #                repo = "example-nix";
    #                rev = "67affc85332894bb8892c9fe98bc9f378d663b90";
    #                sha256 = "1aps3bppzwg9vs9nq3brmxvn6dccwlrwbwq0i37m8k0a1g4446j6";
    #            };
    #    in
    #    import (pkgs-make-path + "/modules/pkgs-make");

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

in

pkgsMake pkgsMakeArgs ({ call, lib }:
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
    rec {

        ekg-assets = call.package modules/ekg-assets;
        example-lib = haskellLib modules/example-lib;
        example-app-static = haskellApp modules/example-app;
        example-app-dynamic =
            lib.haskell.enableSharedExecutables example-app-static;
        example-app-compact = call.package modules/example-app-compact;
        example-tarball = lib.nix.tarball example-app-compact;

        # Values in sub-sets are excluded as dependencies (prevents triggering
        # unnecessary builds when entering into nix-shell).  Be careful not to
        # chose a name that conflicts with a package name in `nixpkgs`.
        #
        example-extra.stack = call.package modules/stack;
        example-extra.licenses =
            lib.nix.licenses.json
                { inherit example-app-compact example-app-dynamic; };

    })
