- [About Attrap Spacemacs Layer](#org0265246)
- [Installation](#org294776c)
- [Features/Configuration](#org4f2f699)



<a id="org0265246"></a>

# About Attrap Spacemacs Layer

This layer provides [Attrap](https://github.com/jyp/attrap) integration for fixing error at the active point.

This layer doesn't do much. It mostly just pulls in the package and sets up a keybinding to the `attrap-attrap` command.


<a id="org294776c"></a>

# Installation

To use this configuration layer, copy or link this layer's `attrap` directory to the private layers directory of your Spacemacs installation keeping the name (`~/.emacs.d/private/attrap`).

Then enable it in your `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/layers ()
  (setq-default
     ;; ... other configuration ...
   dotspacemacs-configuration-layers
     '( ; ... other layers
       attrap)))
```


<a id="org4f2f699"></a>

# Features/Configuration

Attrap Emacs commands have key bindings under the major mode prefix `SPC m`:

| Key Binding | Command         | Description                                        |
|----------- |--------------- |-------------------------------------------------- |
| `SPC m /`   | `attrap-attrap` | attempt to repair a Emacs Lisp defect at the point |

See the [Attrap documentation](https://github.com/jyp/attrap) for more information.
