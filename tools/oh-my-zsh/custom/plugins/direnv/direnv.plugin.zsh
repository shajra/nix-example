_direnv_hook_enabled=false
eval "$(direnv hook zsh)"

_direnv_hook()
{
    if [ "$_direnv_hook_enabled" = "true" ]
    then
        trap -- '' SIGINT
        eval "$(_direnv_export_filtered)"
        trap - SIGINT
    fi
}

_direnv_export_filtered()
{
    if [ -z "$_direnv_hook_debug" ]
    then
         { direnv export zsh 3>&1 1>&2 2>&3 \
             | grep --ignore-case --color=never \
                 '^direnv: load\|error\|fail\|unknown\|unexpected\|warn' \
             | grep --invert-match --ignore-case --color=never \
                 '^direnv: export\|export\|unset'
         } 3>&1 1>&2 2>&3
    else direnv export zsh
    fi
}

direnv-toggle()
{
    if [ "$_direnv_hook_enabled" = "true" ]
    then direnv-disable
    else direnv-enable
    fi
}

direnv-debug-toggle()
{
    if [ "$_direnv_hook_debug" = "true" ]
    then
        echo "direnv: turning off debug output"
        export _direnv_hook_debug="false"
    else
        echo "direnv: turning on debug output"
        export _direnv_hook_debug="true"
    fi
}

direnv-disable()
{
    mkdir -p ~/.cache/direnv/empty
    direnv-freeze ~/.cache/direnv/empty
}

direnv-freeze()
{
    if [ $# -eq 1 ]
    then
        if [ -e "$1/.envrc" ]
        then echo "direnv: setting up shell environment for directory $1"
        fi
        pushd "$1" >/dev/null || return 1
        eval "$(direnv export zsh)"
        _direnv_hook_enabled=false
        popd >/dev/null
    fi
    echo "direnv: disabling shell hook"
    _direnv_hook_enabled=false
}

direnv-thaw()
{
    echo "direnv: enabling shell hook"
    _direnv_hook_enabled=true
}

direnv-enable()
{
    direnv-thaw
}

_direnv_nix_shell="$(whence nix-shell)"
nix-shell()
{
    local old_state="$_direnv_hook_enabled"
    direnv-disable
    "$_direnv_nix_shell" "$@"
    _direnv_hook_enabled="$old_state"
}
if [ -z "$_direnv_nix_shell" ]
then
    unset _direnv_nix_shell
    unfunction nix-shell
fi
