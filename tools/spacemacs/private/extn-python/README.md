- [About Extn-Python Spacemacs Layer](#orga772366)
- [Installation](#org7802702)
  - [Dependencies](#org83e319e)
  - [Enabling the layer](#org1ac67e0)
- [Features/Configuration](#org1c4209d)
  - [Key bindings](#orgf0d8d4c)
    - [From the `python` Spacemacs layer](#org18f6402)
    - [From this layer](#org5587148)



<a id="orga772366"></a>

# About Extn-Python Spacemacs Layer

This layer extends the Spacemacs-provided `python` layer with just an extra keystroke and command to restart the Anaconda server. This restart also picks up Direnv environment changes via the [emacs-direnv](https://github.com/wbolster/emacs-direnv) Emacs package if enabled.

That's all it does for now. Otherwise, it just turns on the `python` layer for you, see [that layer's official documentation](https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/python) for more information.


<a id="org7802702"></a>

# Installation


<a id="org83e319e"></a>

## Dependencies

Spacemacs's `python` layer naturally requires an installation of Python, but also provides support for a variety of Python static analysis tools and test runners.

We want our development experiences to be stable and portable from machine to machine, so it's nice to have project-level Nix expressions provide project-level dependencies. [Pkgs-make](../../../../pkgs-make/README.md) can do this by providing our Python interpretter and development tools for a `nix-shell` session. Nix shell environment variables can be integrated into Direnv with the [Direnv support provided](../../../direnv/README.md) as well as [Oh-My-ZSH integration](../../../oh-my-zsh/README.md). To tie this back to Spacemacs, you can use the [provided `direnv` Spacemacs layer](../direnv/README.md).

Otherwise, you can install any dependencies using an OS package manager like Nix.


<a id="org1ac67e0"></a>

## Enabling the layer

To use this configuration layer, copy or link this layer's `extn-python` directory to the private layers directory of your Spacemacs installation keeping the name (`~/.emacs.d/private/extn-python`).

Then enable it in your `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/layers ()
  (setq-default
     ;; ... other configuration ...
   dotspacemacs-configuration-layers
     '( ; ... other layers
       extn-python)))
```


<a id="org1c4209d"></a>

# Features/Configuration


<a id="orgf0d8d4c"></a>

## Key bindings


<a id="org18f6402"></a>

### From the `python` Spacemacs layer

Many key binds come from the `python` layer. Rather than redocument those bindings here, see the [documentation for that layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/python#key-bindings).


<a id="org5587148"></a>

### From this layer

There's just one command/keybinding we provide:

| Key Binding | Command                             | Description                                             |
|----------- |----------------------------------- |------------------------------------------------------- |
| `SPC m , a` | `extn-python/anaconda-mode-restart` | restart anaconda-server, updating direnv if appropriate |

We could kill the `*anaconda-mode*` buffer to restart the background process, but that's a bit annoying to do without a key binding. `anaconda-mode` tries to intelligently restart itself, which is why they probably don't offer a keybinding for manually restarting.

Additionally, `anaconda-mode` is not aware of when environment variables change from packages like `emacs-direnv`. So our new command also update the Direnv environment variable before restarting the server.
