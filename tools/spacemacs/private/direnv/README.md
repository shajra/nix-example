- [About Direnv Spacemacs Layer](#orgecbd0ec)
- [Installation](#org7c88b95)
  - [Dependencies](#org38a20ea)
  - [Enabling the layer](#org0bf58f1)
- [Features/Configuration](#org452cb5a)



<a id="orgecbd0ec"></a>

# About Direnv Spacemacs Layer

This layer provides [emacs-direnv](https://github.com/wbolster/emacs-direnv)/[Direnv](https://direnv.net/) integration for managing different sets of environment variables for different directories.

This allows, for example, the executables on the `PATH` for one Emacs buffer to be different from another one. One project might use one version of a compiler. Another one might use another installed elsewhere.

With Direnv, we need a lot less language-platform specific configuration with Emacs packages or Spacemacs layers. We just set up the environment the way we want for the command line, and Emacs just picks it up per-buffer. We don't need to launch Emacs from a special environment that only works for certain projects. So direnv is much friendly for Emacs in daemon mode.

This layer just sets up a few key bindings and delegates to emacs-direnv.


<a id="org7c88b95"></a>

# Installation


<a id="org38a20ea"></a>

## Dependencies

You need the `direnv` executable on your `PATH`. There are [many ways to get it](https://github.com/direnv/direnv#install).


<a id="org0bf58f1"></a>

## Enabling the layer

To use this configuration layer, copy or link this layer's `direnv` directory to the private layers directory of your Spacemacs installation keeping the name (`~/.emacs.d/private/direnv`).

Then enable it in your `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/layers ()
  (setq-default
     ;; ... other configuration ...
   dotspacemacs-configuration-layers
     '( ; ... other layers
       direnv)))
```


<a id="org452cb5a"></a>

# Features/Configuration

Some useful Direnv Emacs commands have key bindings under the prefix `SPC d`:

| Key Binding | Command                               | Description                                       |
|----------- |------------------------------------- |------------------------------------------------- |
| `SPC d e`   | `direnv-edit`                         | edit the relevant .envrc file for the buffer      |
| `SPC d d`   | `direnv-update-environment`           | update the environment from direnv for the buffer |
| `SPC d u`   | `direnv-update-directory-environment` | update the environment for the buffer's directory |

See the [emacs-direnv documentation](https://github.com/wbolster/emacs-direnv#usage) for more information.
