let
    pkgsMake = import ../../pkgs-make;
    pkgsMakeArgs = {};
in

pkgsMake pkgsMakeArgs ({call, ...}: {
    example-shell-lib = call.package ./library;
    example-shell-app = call.package ./application-overriding;
})
