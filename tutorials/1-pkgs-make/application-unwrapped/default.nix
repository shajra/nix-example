{ example-shell-lib
, writeShellScriptBin
}:

writeShellScriptBin "example-shell"
    ''
    . ${example-shell-lib}
    do_with_header hello "$@"
    ''