let
    pkgsMake = import ../../pkgs-make;
    pkgsMakeArgs = {};
in

pkgsMake pkgsMakeArgs ({call, ...}: {

    example-shell-lib = call.package ./library;

    example-shell-app = call.package ./application;

    hello = call.package ({stdenv, fetchurl}:
        stdenv.mkDerivation rec {
            name = "hello-2.9";
            src = fetchurl {
                url = "mirror://gnu/hello/${name}.tar.gz";
                sha256 = "19qy37gkasc4csb1d3bdiz9snn8mir2p3aj0jgzmfv0r2hi7mfzc";
            };
        }
    );

})
