- [Introduction](#orgea88321)
- [Installation](#org6fdc48d)
  - [Dependencies](#org8ac7686)
  - [Enabling the layer](#org72ae0b7)
- [Features/Configuration](#orgc2bd968)
  - [Key bindings](#orgf690809)
    - [From the `python` Spacemacs layer](#orgf433d7c)
    - [From this layer](#org45ea544)



<a id="orgea88321"></a>

# Introduction

This layer extends the Spacemacs-provided `python` layer with just an extra keystroke and command to restart the Anaconda server. That's all it does for now. Otherwise, it just turns on the `python` layer for you, see [that layer's official documentation](https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/python) for more information.


<a id="org6fdc48d"></a>

# Installation


<a id="org8ac7686"></a>

## Dependencies

Spacemacs's `python` layer naturally requires an installation of Python, but also provides support for a variety of Python static analysis tools and test runners.

This layer provides a small amount of support for [Direnv](https://direnv.net/) and [emacs-direnv](https://github.com/wbolster/emacs-direnv) if you choose to use these.


<a id="org72ae0b7"></a>

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


<a id="orgc2bd968"></a>

# Features/Configuration


<a id="orgf690809"></a>

## Key bindings


<a id="orgf433d7c"></a>

### From the `python` Spacemacs layer

Many key binds come from the `python` layer. Rather than redocument those bindings here, see the [documentation for that layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/python#key-bindings).


<a id="org45ea544"></a>

### From this layer

There's just one command/keybinding we provide:

| Key Binding | Description                                             |
|----------- |------------------------------------------------------- |
| `SPC m , a` | restart anaconda-server, updating direnv if appropriate |

We could kill the `*anaconda-mode*` buffer to restart the background process, but that's a bit annoying to do without a key binding. `anaconda-mode` tries to intelligently restart itself, which is why they probably don't offer a keybinding for manually restarting.

Additionally, `anaconda-mode` is not aware of when environment variables change from packages like `emacs-direnv`. So our new command also update the Direnv environment variable before restarting the server.
