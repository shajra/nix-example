- [About Extn-Spacemacs Spacemacs Layer](#sec-1)
- [Installation](#sec-2)
- [Features/Configuration](#sec-3)
  - [Global fill-column indicator mode](#sec-3-1)
  - [Title abbreviation](#sec-3-2)
  - [Multiple Xref backend simultaneously](#sec-3-3)


# About Extn-Spacemacs Spacemacs Layer<a id="sec-1"></a>

This layer extends what's already in Emacs and Spacemacs without bringing in any new packages.

The following features are provided

-   A global fill-column-indicator mode that skips special Emacs buffers
-   A small function to abbreviate titles slightly
-   A hack for enabling multiple Xref backends simultaneously.

# Installation<a id="sec-2"></a>

To use this configuration layer, copy or link this layer's `extn-spacemacs` directory to the private layers directory of your Spacemacs installation keeping the name (`~/.emacs.d/private/extn-spacemacs`).

Then enable it in your `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/layers ()
  (setq-default
     ;; ... other configuration ...
   dotspacemacs-configuration-layers
     '( ; ... other layers
       (extn-spacemacs :variables
	;; layer settings in pairs; for example...
	extn-spacemacs/xref-backends-fallingback t))))
```

# Features/Configuration<a id="sec-3"></a>

## Global fill-column indicator mode<a id="sec-3-1"></a>

The `fill-column-indicator` package is already pulled in by Spacemacs, and provides `fci-mode`, which gives a thin margin at your `fill-column` setting. That mode can be turned on per-hooks. This layer offers a global mode that turns on this margin for all buffers execpt for special Emacs buffers with names bracketed by the ‘\*’ character.

You can enable this global mode with the following configuration in `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/user-config ()
  ;; ... other configuration ...
  (global-fci-mode))
```

## Title abbreviation<a id="sec-3-2"></a>

The window titles of buffers can get long, and it's nice to abbrevate things like “/home/$USER/” to just “~”. You can do that with the following configuration in `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/user-config ()
  ;; ... other configuration ...
  (extn-spacemacs/setq-default-frame-title-format-enhanced))
```

## Multiple Xref backend simultaneously<a id="sec-3-3"></a>

Emacs' Xref support allows multiple backends to be queried, but only one is selected. This layer provides an experimental hack for allowing multiple backends to be used in a cascade.

See the Emacs docstring for `extn-spacemacs/xref-backends-fallingback` for more information.

Enable this feature with the `extn-spacemacs/xref-backends-fallingback` layer configuration in `~/.spacemacs` (after the `:variables` keyword).
