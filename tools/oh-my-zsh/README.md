- [About Direnv Oh-My-ZSH Integration](#sec-1)
- [Installation](#sec-2)
  - [Dependencies](#sec-2-1)
  - [Installing the plugin](#sec-2-2)
  - [Installing a Direnv-aware theme](#sec-2-3)
- [Usage](#sec-3)


# About Direnv Oh-My-ZSH Integration<a id="sec-1"></a>

This provides integration of [Direnv](https://direnv.net/) and [Z Shell (Zsh)](https://www.zsh.org/) via an [Oh-My-ZSH](https://ohmyz.sh) plugin and theme.

Direnv provides a nice way to manage environment variables by project directory. By just changing into a directory in a terminal, your environment variables can automatically change, including things like `PATH` and language specific paths like `PYTHONPATH` (Direnv is very flexible and can be scripted for almost anything).

Zsh is a shell replacement that some people prefer for its richer built-in and community-curated scripts and themes. Oh-My-ZSH is a popular and example of this curation. Oh-My-ZSH is also modular. This integration is just a plugin and theme for it.

The provided plugin also works well with Nix's `nix-shell` tool, turning off Direnv management before entering nix-shell to avoid a circular problem if Direnv is pulling its environment variables from `nix-shell`.

# Installation<a id="sec-2"></a>

## Dependencies<a id="sec-2-1"></a>

As you may have guessed, for this integration to work you must perform the following steps first:

1.  [install Zsh as your shell](https://github.com/robbyrussell/oh-my-zsh/wiki/Installing-ZSH)
2.  [install Oh-My-ZSH](https://github.com/robbyrussell/oh-my-zsh#manual-installation)
3.  [install Direnv](https://github.com/direnv/direnv#install).

## Installing the plugin<a id="sec-2-2"></a>

Copy or link the provided `direnv.plugin.zsh` file to the `custom/plugins` directory of your Oh-My-ZSH installation, which by default is `~/.oh-my-zsh/custom/plugins`.

Then activate the plugin in your `~/.zshrc` file:

```zsh
# must set before sourcing oh-my-zsh.sh
plugins=(
    # ... other plugins ...
    direnv)

source "$ZSH/oh-my-zsh.sh"  # ZSH is typically ~/.oh-my-zsh
```

## Installing a Direnv-aware theme<a id="sec-2-3"></a>

It's easy to forget if you're in a directory with environment variables altered by this plugin, so it's nice to install a theme that gives some indication with an altered prompt. An example of one using the `DIRENV_DIR` variable is provided. You can look at how it's designed to make your own or use it directly.

To use it, copy or link the provided `tnks.zsh-theme` file to the `custom/themes` directory of your Oh-My-ZSH installation (normally `~/.oh-my-zsh/custom/themes`).

Then activate the theme in your `~/.zshrc` file:

```zsh
ZSH_THEME=tnks  # set before sourcing oh-my-zsh.sh

# ... other oh-my-zsh configuration like plugins ...

source "$ZSH/oh-my-zsh.sh"  # ZSH is typically ~/.oh-my-zsh
```

# Usage<a id="sec-3"></a>

The plugin provides some functions and sets up your Zsh prompt with a hook to integrate with these commands. Here are the following functions available:

| Command           | Description                                             |
|----------------- |------------------------------------------------------- |
| direnv-enable     | turns on environment auto-changing for PWD              |
| direnv-disable    | turns off environment auto-changing for PWD             |
| direnv-toggle     | toggles between direnv-enable and direnv-disable        |
| direnv-freeze DIR | sets environment for DIR and disables PWD auto-changing |
| direnv-thaw       | alias of direnv-enable (mnemonic opposite for freezing) |

None of these commands do anything until you configure directories for use with Direnv by putting a `.envrc` file in them, and then enabling the directory for use with a `direnv allow` call. See the [Direnv documentation](https://direnv.net/) for details.
