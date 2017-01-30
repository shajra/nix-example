let

    nixpkgsVersion = {
        # git describe: 16.09-beta-8886-g92b1e39e1c
        rev = "92b1e39e1c43a55a1460571782e3c7444556814b";
        sha256 = "1y62n4h70kh9a0pyigssfwfrdc1kfx9i4743722f2c6kf67j1v20";
    };
    call = (import modules/nix/call) nixpkgsVersion;
    pkgs = call ({ call, ... }: {
        stack = call.package modules/stack;
        ekg-assets = call.package modules/ekg-assets;
        strings-replace = call.package modules/nix/strings-replace;
        example-lib = call.haskell.lib modules/lib;
        example-app = call.haskell.app modules/app;
        example-bundle = call.package modules/bundle;
    });

in

pkgs
