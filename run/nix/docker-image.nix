{ common-sh
, coreutils
, docker
, nix-project-lib
}:

{ tutorialName
, tutorialNixFile
, executable
}:

let
    prog_name = "run_${tutorialName}_docker-image_docker-unused";
    desc = "Run Docker image built for the \"${tutorialName}\" tutorial";
in

nix-project-lib.writeShellChecked prog_name desc
''
set -eu
set -o pipefail


. "${nix-project-lib.lib-sh}/bin/lib.sh"
. "${common-sh}/bin/common.sh"


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
    local attr="${executable}-docker"
    local image="${executable}"

    intro '${desc}'
    add_nix_to_path "$NIX_EXE"

    log "This script illustrates commands to build with Nix a Docker image" \
        "for the example executable of the \"${tutorialName}\" tutorial." \
        "We then run this image as a Docker container."

    log "All of these commands are run from this project's root directory."

    log "To generate a compressed (tarballed) Docker image, we build the" \
	"\"$attr\" attribute of the set we get from evaluating the expression" \
        "in the $(prune_path ${tutorialNixFile}) Nix file:"
    log_and_run_silently nix build --show-trace --no-link \
        --file "${tutorialNixFile}" "$attr"

    log "We can get the location of the build with 'nix path-info':"
    log_and_run nix path-info --file "${tutorialNixFile}" "$attr"

    log "We can now load this image file into Docker:"
    log_and_run ${docker}/bin/docker load --input "$(
        nix path-info --file "${tutorialNixFile}" "$attr"
    )"

    log "And finally we can run the image as a Docker container:"
    log_and_run ${docker}/bin/docker run --rm -it "$image"
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
