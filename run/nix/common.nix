# Common functions useful for multiple scripts
{ coreutils
, gnused
, nix-project-lib
}:

nix-project-lib.writeShellChecked "common.sh"
"Common functions for example-nix scripts"
''
do_with_header()
{
    echo; echo
    echo "*****"
    for token in "$@"
    do echo "***** $token"
    done
    echo "*****"
    echo
    "$@"
}
''
