# Common functions useful for multiple scripts
{ coreutils
, gnused
, nix-project-lib
}:

nix-project-lib.writeShellChecked "common.sh"
"Common functions for example-nix scripts"
''
intro()
{
    echo
    echo "$@"
    echo "$@" | ${coreutils}/bin/tr -c '\n' =
    echo
}

log()
{
    echo "$@" | ${coreutils}/bin/fold --spaces --width 65
    echo
}

log_command()
{
    if [ "$(pruned_length "$*")" -lt 60 ]
    then echo "    $(prune_path "$*")"
    else
        echo "    $(prune_path "$1") \\"; shift
        while [ "$#" -gt 1 ]
        do echo "        $(prune_path "$1") \\"; shift
        done
        if [ $# -eq 1 ]
        then echo "        $(prune_path "$1")"
        fi
    fi
    echo
}

pruned_length()
{
    prune_path "$*" | ${coreutils}/bin/wc --chars
}

log_and_run()
{
    log_command "$@"
    echo ' |'
    "$@" | ${gnused}/bin/sed -e 's/^/ | /'
    echo ' |'
    echo
}

log_and_run_silently()
{
    log_command "$@"
    "$@" > /dev/null
}

prune_path()
{
    local without_store="''${1#/nix/store/*/}"
    echo "''${without_store#bin/}"
}

''
