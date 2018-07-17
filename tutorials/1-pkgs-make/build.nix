let
    pkgsMake = import ../../pkgs-make;
    pkgsMakeArgs = {};
in

pkgsMake pkgsMakeArgs ({call, lib}: rec {

    example-shell-lib = call.package ./library;

    example-shell-app = call.package ./application;

    example-shell-app-unwrapped = call.package ./application-unwrapped;

    example-shell-app-wrapped = call.package ./application-wrapped;

    example-shell-docker = lib.nix.dockerTools.buildImage {
        name = "example-shell";
        contents = example-shell-app;
        config = {
            Entrypoint = [ "/bin/example-shell" ];
        };
    };

    example-shell-tarball =
        lib.nix.tarball example-shell-app "example-shell.tar";

    example-shell-licenses = lib.nix.license-report.json {
        inherit example-shell-app;
    };

})
