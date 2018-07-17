_tnks_set_theme()
{
    _tnks_set_git_theme
    _tnks_set_prompt
}

_tnks_set_git_theme()
{
    ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}"
    ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
    ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%} clean"
    ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} dirty:"
    ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[cyan]%}A"
    ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%}M"
    ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%}D"
    ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[blue]%}R"
    ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[magenta]%}U"
    ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[grey]%}?"
}

_tnks_set_prompt()
{
    if [[ -n "$SSH_CLIENT"  ||  -n "$SSH2_CLIENT" ]]
    then
        local host='%{$fg[yellow]%}%M%{$reset_color%}'
    else
        local host='%{$fg[green]%}%M%{$reset_color%}'
    fi

    local user='%{$fg[green]%}%n%{$reset_color%}'
    local return_code='%(?..%{$fg[red]%}last:%?%{$reset_color%})'
    local direnv='$(_tnks_direnv_prompt)'
    local prompt_arrow='%{$reset_color%}$%{$reset_color%}'
    local current_dir='%{$fg_bold[blue]%}%~%{$reset_color%}'
    local git_branch='$(git_prompt_info)$(git_prompt_status)%{$reset_color%}'

    PROMPT=$'\a\n'"$user $host $current_dir$direnv $git_branch $return_code"$'\n'"$prompt_arrow "
}

_tnks_direnv_prompt()
{
    if [ "$DIRENV_DIR" = "" ]
    then echo ""
    else echo " ($(basename "${DIRENV_DIR#-}"))"
    fi
}


_tnks_set_theme
