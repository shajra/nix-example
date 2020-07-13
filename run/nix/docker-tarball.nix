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
    prog_name = "run_${tutorialName}_docker-tarball_docker-unused";
    desc = "Build tarball for \"${tutorialName}\" tutorial and run with Docker";
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
    local attr="${executable}-tarball"
    local image="${executable}-tarball"
    local context; context="$(${coreutils}/bin/mktemp -d)"
    local context_tarball="$context/${executable}.tar"
    local root; root="$(${coreutils}/bin/dirname ${tutorialNixFile})"
    local dockerfile="$root/Dockerfile"

    # DESIGN: expansion now is desired
    # shellcheck disable=SC2064
    trap "${coreutils}/bin/rm -rf $context" EXIT

    intro '${desc}'
    add_nix_to_path "$NIX_EXE"

    log "This script illustrates commands to build with Nix a Docker image" \
        "for the example executable of the \"${tutorialName}\" tutorial." \
	"This image is not built directly with Nix.  Instead we build a" \
        "tarball with Nix and then use Docker to make an image from it." \
        "We then run this image as a Docker container."

    log "All of these commands are run from this project's root directory."

    log "First we build the \"$attr\" attribute of the set we get from" \
        "evaluating the expression in the" \
        "$(prune_path ${tutorialNixFile}) Nix file:"
    log_and_run_silently nix build --show-trace --no-link \
        --file "${tutorialNixFile}" "$attr"

    log "We can get the location of the build with 'nix path-info':"
    log_and_run nix path-info --file "${tutorialNixFile}" "$attr"

    log "The tutorial includes a Dockerfile we can use to build a Docker" \
        "image from this tarball.  We'll do this in a temporary directory:"
    log_and_run_silently ${coreutils}/bin/mkdir --parents "$context"

    log "We'll copy the Dockerfile and tarball into this temporary directory:"
    log_and_run_silently ${coreutils}/bin/cp "$dockerfile" "$context"
    log_and_run_silently ${coreutils}/bin/cp "$(
        nix path-info --file "${tutorialNixFile}" "$attr"
    )" "$context_tarball"

    log "We can now build our Docker image:"
    log_and_run ${docker}/bin/docker build --tag "$image" "$context"

    log "And finally we can run the image as a Docker container:"
    log_and_run ${docker}/bin/docker run --rm -it "$image"

    log "Lastly, we can clean up our temporary directory:"
    log_and_run_silently ${coreutils}/bin/rm --recursive --force "$context"
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
