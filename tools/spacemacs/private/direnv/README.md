- [Introduction](#org8a3ecb6)
- [Installation](#org0be1321)
  - [Dependencies](#org7654d8d)
  - [Enabling the layer](#orgfa55044)
- [Features/Configuration](#org58714b7)



<a id="org8a3ecb6"></a>

# Introduction

This layer provides [emacs-direnv](https://github.com/wbolster/emacs-direnv)/[Direnv](https://direnv.net/) integration for managing different sets of environment variables for different directories.

This allows, for example, the executables on the `PATH` for one Emacs buffer to be different from another one. One project might use one version of a compiler. Another one might use another installed elsewhere.

With Direnv, we need a lot less language-platform specific configuration with Emacs packages or Spacemacs layers. We just set up the environment the way we want for the command line, and Emacs just picks it up per-buffer. We don't need to launch Emacs from a special environment that only works for certain projects. So direnv is much friendly for Emacs in daemon mode.

This layer doesn't do much. It just sets up a few key bindings.


<a id="org0be1321"></a>

# Installation


<a id="org7654d8d"></a>

## Dependencies

You need the `direnv` executable on your `PATH`. There are [many ways to get it](https://github.com/direnv/direnv#install).


<a id="orgfa55044"></a>

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


<a id="org58714b7"></a>

# Features/Configuration

All the important direnv Emacs commands have key bindings under the prefix `SPC d`:

| Key Binding | Description                                       |
|----------- |------------------------------------------------- |
| `SPC d e`   | edit the relevant .envrc file for the buffer      |
| `SPC d d`   | update the environment from direnv for the buffer |
| `SPC d u`   | update the environment for the buffer's directory |

See the [emacs-direnv documentation](https://github.com/wbolster/emacs-direnv#usage) for more information.
