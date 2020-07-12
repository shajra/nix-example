{ coreutils
, docker
, less
, most
, nixFile
, nix-project-lib
}:

let
    prog_name = "example-nix-run";
    desc = "Run an example from an example-nix tutorial";
in

nix-project-lib.writeShellChecked prog_name
desc
''
set -eu
set -o pipefail


COMMAND=nix-run
DOCKER=false
PAGER=true
TUTORIAL=pkgs-make


. "${nix-project-lib.lib-sh}/bin/lib.sh"


print_usage()
{
    ${coreutils}/bin/cat - <<EOF
USAGE: ${prog_name} [OPTION] COMMAND

DESCRIPTION:

    ${desc}

OPTIONS:

    -h --help           print this help message
    -d --docker         run command inside a Docker container
    -t --tutorial NAME  tutorial to run code from
    -N --nix PATH       filepath of 'nix' executable to use
    -P --no-pager       don't pipe through a pager

    Tutorials must be one of the following:

       pkgs-make (default)
       haskell
       python

COMMANDS:

    nix-run (default)
        run with 'nix run'

    docker-image
        build a Docker image with Nix and run it

    docker-tarball
        build a Docker image from a tarball built by Nix
        and run it

    licenses
        print license report

EOF
}

main()
{
    parse_args "$@"
    validate_args
    if "$PAGER"
    then nix_run 2>&1 \
        | ${less}/bin/less +G \
        --raw-control-chars --quit-if-one-screen --LONG-PROMPT
    else nix_run
    fi
}

nix_run()
{
    add_nix_to_path "$NIX_EXE"
    local suffix=docker-unused
    if "$DOCKER"
    then suffix=docker-used
    fi
    exec nix -L run --show-trace \
        --ignore-environment \
        --keep LANG \
        --keep TERM \
        --keep SSL_CERT_FILE \
        --file "${nixFile}" \
        "run.$TUTORIAL.$COMMAND.$suffix" \
        --command "run_''${TUTORIAL}_''${COMMAND}_$suffix" \
        --nix \
        "$(command -v nix)"
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
        -d|--docker)
            DOCKER=true
            ;;
        -P|--no-pager)
            PAGER=false
            ;;
        -t|--tutorial)
            TUTORIAL="''${2:-}"
            if [ -z "$TUTORIAL" ]
            then die "'$1' requires argument"
            fi
            shift
            ;;
        -N|--nix)
            NIX_EXE="''${2:-}"
            if [ -z "$NIX_EXE" ]
            then die "'$1' requires argument"
            fi
            shift
            ;;
        nix-run|docker-image|docker-tarball|licenses)
            COMMAND="''${1:-}"
            ;;
        *)
            die "'$1' not recognized"
            ;;
        esac
        shift
    done
}

validate_args()
{
    case "$COMMAND" in
    nix-run|licenses)
        ;;
    docker-image|docker-tarball)
        if "${if nix-project-lib.isDarwin then "true" else "false"}" && ! "$DOCKER"
        then die "'$COMMAND' on MacOS must be run with --docker"
        fi
        validate_docker "the '$COMMAND' command"
        ;;
    *)
        die "'$COMMAND' is not a valid command"
        ;;
    esac

    if "$DOCKER"
    then validate_docker "the '--docker' switch"
    fi

    case "$TUTORIAL" in
    pkgs-make|haskell|python)
        ;;
    *)
        die "'$TUTORIAL' is not a valid tutorial option"
        ;;
    esac
}

validate_docker()
{
    if ! ${docker}/bin/docker ps >/dev/null 2>&1
    then die "Docker required for $1, but not running"
    fi
}


main "$@"
''
