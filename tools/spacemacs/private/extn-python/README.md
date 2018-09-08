- [About Extn-Python Spacemacs Layer](#sec-1)
- [Installation](#sec-2)
  - [Dependencies](#sec-2-1)
  - [Enabling the layer](#sec-2-2)
- [Features/Configuration](#sec-3)
  - [Key bindings](#sec-3-1)
    - [From the `python` Spacemacs layer](#sec-3-1-1)
    - [From this layer](#sec-3-1-2)


# About Extn-Python Spacemacs Layer<a id="sec-1"></a>

This layer extends the Spacemacs-provided `python` layer with just an extra keystroke and command to restart the Anaconda server. This restart also picks up Direnv environment changes via the [emacs-direnv](https://github.com/wbolster/emacs-direnv) Emacs package if enabled.

That's all it does for now. Otherwise, it just turns on the `python` layer for you, see [that layer's official documentation](https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/python) for more information.

# Installation<a id="sec-2"></a>

## Dependencies<a id="sec-2-1"></a>

Spacemacs's `python` layer naturally requires an installation of Python, but also provides support for a variety of Python static analysis tools and test runners.

We want our development experiences to be stable and portable from machine to machine, so it's nice to have project-level Nix expressions provide project-level dependencies. [Pkgs-make](../../../../pkgs-make/README.md) can do this by providing our Python interpretter and development tools for a `nix-shell` session. Nix shell environment variables can be integrated into Direnv with the [Direnv support provided](../../../direnv/README.md) as well as [Oh-My-ZSH integration](../../../oh-my-zsh/README.md). To tie this back to Spacemacs, you can use the [provided `direnv` Spacemacs layer](../direnv/README.md).

Otherwise, you can install any dependencies using an OS package manager like Nix.

## Enabling the layer<a id="sec-2-2"></a>

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

# Features/Configuration<a id="sec-3"></a>

## Key bindings<a id="sec-3-1"></a>

### From the `python` Spacemacs layer<a id="sec-3-1-1"></a>

Many key binds come from the `python` layer. Rather than redocument those bindings here, see the [documentation for that layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/python#key-bindings).

### From this layer<a id="sec-3-1-2"></a>

There's just one command/keybinding we provide:

| Key Binding | Command                             | Description                                             |
|----------- |----------------------------------- |------------------------------------------------------- |
| `SPC m , a` | `extn-python/anaconda-mode-restart` | restart anaconda-server, updating direnv if appropriate |

We could kill the `*anaconda-mode*` buffer to restart the background process, but that's a bit annoying to do without a key binding. `anaconda-mode` tries to intelligently restart itself, which is why they probably don't offer a keybinding for manually restarting.

Additionally, `anaconda-mode` is not aware of when environment variables change from packages like `emacs-direnv`. So our new command also update the Direnv environment variable before restarting the server.
