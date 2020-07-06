let

    projectRoot = pkgs.lib.cleanSource ../..;

    sources = import ../../support/nix/sources.nix;

    pkgs = import sources.nixpkgs {
        config = {};
        overlays = [overlay];
    };

    overlay = self: super: {
        common-sh = self.callPackage
            (import "${projectRoot}/run/nix/common.nix") {};
    } // (import sources.nix-project);

    commands = {
        nix-run = "${projectRoot}/run/nix/nix-run.nix";
        docker-image = "${projectRoot}/run/nix/docker-image.nix";
        docker-tarball = "${projectRoot}/run/nix/docker-tarball.nix";
        licenses = "${projectRoot}/run/nix/licenses.nix";
    };

    tutorials = {
        pkgs-make = "${projectRoot}/tutorials/1-pkgs-make/build.nix";
        haskell = "${projectRoot}/tutorials/2-haskell/build.nix";
        python = "${projectRoot}/tutorials/2-python/build.nix";
    };

    executables = {
        pkgs-make = "example-shell";
        haskell = "example-haskell";
        python = "example-python";
    };

    builds = ["docker-used" "docker-unused"];

    genCommands = tutorial: pkgs.recurseIntoAttrs
        (pkgs.lib.genAttrs (builtins.attrNames commands) (genBuilds tutorial));

    genBuilds = tutorial: command: pkgs.recurseIntoAttrs
        (pkgs.lib.genAttrs builds (genBuild tutorial command));

    genBuild = tutorial: command: build:
        if build == "docker-used"
        then pkgs.callPackage (import "${projectRoot}/run/nix/run-docker.nix") {
            inherit tutorial command;
        }
        else pkgs.callPackage (import (commands."${command}")) {} {
            tutorialName = tutorial;
            tutorialNixFile = tutorials."${tutorial}";
            executable = executables."${tutorial}";
        };

    run = pkgs.recurseIntoAttrs
        (pkgs.lib.genAttrs (builtins.attrNames tutorials) genCommands);

    run-unified = pkgs.callPackage (import "${projectRoot}/run/nix/run.nix") {
        nixFile = "${projectRoot}/run/nix";
    };

in {
    inherit pkgs;
    run = run // { unified = run-unified; };
}
