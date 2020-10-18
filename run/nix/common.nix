# Common functions useful for multiple scripts
{ coreutils
, gnused
, nix-project-lib
}:

nix-project-lib.writeShellChecked "common.sh"
"Common functions for nix-package scripts"
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
    if [ "$(pruned_length "$@")" -lt 60 ]
    then echo "    $(prune_paths "$@")"
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
    "$@" | ${gnused}/bin/sed -u -e 's/^/ | /'
    echo ' |'
    echo
}

log_and_run_unindented()
{
    log_command "$@"
    "$@"
    echo
}

log_and_run_silently()
{
    log_command "$@"
    "$@" > /dev/null
}

prune_paths()
{
    printf "%s" "$(prune_path "$1")"; shift
    while [ "$#" -gt 1 ]
    do printf "%s" " $(prune_path "$1")"; shift
    done
    if [ $# -eq 1 ]
    then echo " $(prune_path "$1")"
    fi
}

prune_path()
{
    local prune_1="''${1#/nix/store/*-nix-package/}"
    local prune_2="''${prune_1#/nix/store/*-mnt/}"
    local prune_3="''${prune_2#/nix/store/*/bin/}"
    echo "$prune_3"
}
''
