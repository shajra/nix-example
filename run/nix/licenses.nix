{ common-sh
, coreutils
, jq
, nix-project-lib
}:

{ tutorialName
, tutorialNixFile
, executable
}:

let
    prog_name = "run_${tutorialName}_licenses_docker-run-unused";
    desc = "Licenses report for the \"${tutorialName}\" tutorial";
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
    local attr="${executable}-licenses"

    intro '${desc}'
    add_nix_to_path "$NIX_EXE"

    log "This script illustrates commands to build a license report for" \
        "the \"${tutorialName}\" tutorial. These commands are run from" \
        "this project's root directory"

    log "First we build the \"$attr\" attribute of the set we get from" \
        "evaluating the expression in the" \
        "$(prune_path ${tutorialNixFile}) Nix file:"
    log_and_run_silently nix build --show-trace --no-link \
        --file "${tutorialNixFile}" "$attr"

    log "We can get the location of the build with 'nix path-info':"
    log_and_run nix path-info --file "${tutorialNixFile}" "$attr"

    log "Here's the output of that file showing licenses for dependencies" \
        "in a JSON format:"
    log_and_run ${jq}/bin/jq --color-output --unbuffered . "$(
        nix path-info --file "${tutorialNixFile}" "$attr"
    )"
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
