{ writeShellScriptBin
, example-shell-lib
, stdenv
, fetchurl
}:

let

    hello = stdenv.mkDerivation rec {
        name = "hello-2.9";
        src = fetchurl {
            url = "mirror://gnu/hello/${name}.tar.gz";
            sha256 = "19qy37gkasc4csb1d3bdiz9snn8mir2p3aj0jgzmfv0r2hi7mfzc";
        };
    };

in

writeShellScriptBin "example-shell"
    ''
    . ${example-shell-lib}
    do_with_header ${hello}/bin/hello "$@"
    ''
