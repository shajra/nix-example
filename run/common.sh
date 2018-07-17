# Common functions useful for multiple scripts


USAGE="$0 [--help ] (shell | haskell | python)"

export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/14a9ca27e69e33ac8ffb708de08883f8079f954a.tar.gz


run()
(
    cmd="$1"
    project="$2"
    case "$project" in
       -h|--help) echo "$USAGE"; exit 0;;
       shell|python|haskell) "$cmd" "$project";;
       "") die "no argument supplied";;
       *) die "bad argument: $project";;
    esac
)

abspath()
{
    local target="$1"
    local old_pwd
    old_pwd="$(pwd)"
    cd "$(dirname "$target")"
    while [ -L "$target" ]; do
        target="$(readlink "$target")"
        cd "$(dirname "$target")"
    done
    echo "$(pwd -P)/$(basename "$target")"
    cd "$old_pwd"
}

die()
(
    msg="$1"
    { echo "$USAGE"; echo "ERROR: $msg"; } >&2
    exit 1
)

do_with_header()
{
    echo; echo
    echo "***** $@" | sed 's/ /\n***** /g'
    echo "*****"
    echo
    "$@"
}

project_root()
(
    project_type="$1"
    repo_root="$(repository_root "$0")"
    case "$project_type" in
        haskell) abspath "$repo_root/tutorials/2-haskell";;
        python) abspath "$repo_root/tutorials/2-python";;
        shell) abspath "$repo_root/tutorials/1-pkgs-make";;
    esac
)

repository_root()
(
    abspath "$(dirname "$0")/.."
)
