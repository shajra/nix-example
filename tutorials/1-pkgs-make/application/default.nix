{ example-shell-lib
, hello
, writeShellScriptBin
}:

writeShellScriptBin "example-shell"
    ''
    . ${example-shell-lib}
    do_with_header ${hello}/bin/hello "$@"
    ''
