let

    pkgsMake = import ../../pkgs-make;

    pkgsMakeArgs.haskellArgs = {
        pkgChanges = lib: {
            example-haskell-lib-nodoc-2 = [ lib.haskell.dontHaddock ];
        };
        changePkgs = {
            dontHaddock = [ "example-haskell-lib-doc-3" ];
        };
    };

in

pkgsMake pkgsMakeArgs ({ call, lib, ... }: rec {

    example-haskell-lib = call.haskell.lib ./library;

    example-haskell-app = call.haskell.app ./application;

    example-haskell-lib-nodoc-1 =
        lib.haskell.dontHaddock example-haskell-lib;

    example-haskell-lib-nodoc-2 = example-haskell-lib;

    example-haskell-lib-nodoc-3 = example-haskell-lib;

    example-haskell-docker = lib.nix.dockerTools.buildImage {
        name = "example-haskell";
        contents = example-haskell-app;
        config = {
            ExposedPorts = { "8081/tcp" = {}; };
            Entrypoint = [ "/bin/example-haskell" ];
        };
    };

    example-haskell-tarball =
        lib.nix.tarball example-haskell-app "example-haskell.tar";

    example-haskell-licenses = lib.nix.license-report.json {
        inherit example-haskell-app;
    };

    example-haskell-stack = call.package ./stack;

})
