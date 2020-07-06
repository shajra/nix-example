{ coreutils
, nix-project-lib
, nixFile
}:

let
    prog_name = "run";
    desc = "Run app from an example=nix tutorial";
in

nix-project-lib.writeShellChecked "example-nix-run"
desc
''
set -eu


DOCKER=false
COMMAND=nix-run
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

    Tutorials must be one of the following:

       pkgs-make (default)
       haskell
       python

COMMANDS:

    nix-run
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
    add_nix_to_path "$NIX_EXE"
    local suffix=docker-unused
    if "$DOCKER"
    then suffix=docker-used
    fi
    exec nix run --show-trace \
        --ignore-environment \
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
            exit 0
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
        if "${builtins.toString nix-project-lib.isDarwin}" && ! "$DOCKER"
        then die "'$COMMAND' on MacOS must be run with --docker"
        fi
        ;;
    *)
        die "'$COMMAND' is not a valid command"
        ;;
    esac
    case "$TUTORIAL" in
    pkgs-make|haskell|python)
        ;;
    *)
        die "'$TUTORIAL' is not a valid tutorial option"
        ;;
    esac
}


main "$@"
''
