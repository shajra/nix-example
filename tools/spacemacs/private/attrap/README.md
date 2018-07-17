- [Introduction](#org89ffe41)
- [Installation](#org7133ae3)
- [Features/Configuration](#orgc740dab)



<a id="org89ffe41"></a>

# Introduction

This layer provides [Attrap](https://github.com/jyp/attrap) integration for fixing error at the active point.

This layer doesn't do much. It mostly just pulls in the package and sets up a keybinding to the `attrap-attrap` command.


<a id="org7133ae3"></a>

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


<a id="orgc740dab"></a>

# Features/Configuration

All the important direnv Emacs commands have key bindings under the major mode prefix `SPC m`:

| Key Binding | Description                                        |
|----------- |-------------------------------------------------- |
| `SPC m /`   | attempt to repair a Emacs Lisp defect at the point |

See the [Attrap documentation](https://github.com/jyp/attrap) for more information.
