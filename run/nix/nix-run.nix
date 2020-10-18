{ coreutils
, common-sh
, nix-project-lib
}:

{ tutorialName
, tutorialNixFile
, executable
}:

let
    prog_name = "run_${tutorialName}_nix-run_docker-run-unused";
    desc = "Run example in the \"${tutorialName}\" tutorial with \"nix run\"";
in

nix-project-lib.writeShellChecked prog_name desc
''
set -eu
set -o pipefail


. "${nix-project-lib.lib-sh}/bin/lib.sh"
. "${common-sh}/bin/common.sh"


NIX_EXE="$(command -v nix || true)"


print_usage()
{
    ${coreutils}/bin/cat - <<EOF
USAGE: ${prog_name} [OPTION] COMMAND

DESCRIPTION:

    ${desc}

OPTIONS:

    -h --help           print this help message
    -N --nix PATH       filepath of 'nix' executable to use

EOF
}

main()
{
    parse_args "$@"
    local attr="${executable}-app"

    intro '${desc}'
    add_nix_to_path "$NIX_EXE"

    log "This script illustrates the command to build and run the" \
        "\"${executable}\" executable of the \"${tutorialName}\" tutorial." \
        "This command is run from this project's root directory."

    log "The 'nix run' call builds the \"$attr\" attribute of the set we get" \
        "from evaluating the expression in the" \
        "$(prune_path ${tutorialNixFile}) Nix file. This gives us a" \
        "derivation for a Nix package that provides the \"${executable}\"" \
	"executable.  The same call then puts this package's executable on" \
        "the PATH and runs it:"

    log_and_run nix run --show-trace \
        --ignore-environment \
        --file "${tutorialNixFile}" \
        "$attr" \
        --command \
        "${executable}"
}

parse_args()
{
    while ! [ "''${1:-}" = "" ]
    do
        case "$1" in
        -h|--help)
            print_usage
            exit 0
            ;;
        -N|--nix)
            NIX_EXE="''${2:-}"
            if [ -z "$NIX_EXE" ]
            then die "'$1' requires argument"
            fi
            shift
            ;;
        *)
            die "'$1' not recognized"
            ;;
        esac
        shift
    done
}

main "$@"
''
