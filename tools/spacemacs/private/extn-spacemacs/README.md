- [About Extn-Spacemacs Spacemacs Layer](#org085a9b9)
- [Installation](#org4deb215)
- [Features/Configuration](#org5c5efe0)
  - [Global fill-column indicator mode](#org9d6b95d)
  - [Title abbreviation](#org8d3ae79)
  - [Multiple Xref backend simultaneously](#org2eefeb6)



<a id="org085a9b9"></a>

# About Extn-Spacemacs Spacemacs Layer

This layer extends what's already in Emacs and Spacemacs without bringing in any new packages.

The following features are provided

-   A global fill-column-indicator mode that skips special Emacs buffers
-   A small function to abbreviate titles slightly
-   A hack for enabling multiple Xref backends simultaneously.


<a id="org4deb215"></a>

# Installation

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


<a id="org5c5efe0"></a>

# Features/Configuration


<a id="org9d6b95d"></a>

## Global fill-column indicator mode

The `fill-column-indicator` package is already pulled in by Spacemacs, and provides `fci-mode`, which gives a thin margin at your `fill-column` setting. That mode can be turned on per-hooks. This layer offers a global mode that turns on this margin for all buffers execpt for special Emacs buffers with names bracketed by the ‘\*’ character.

You can enable this global mode with the following configuration in `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/user-config ()
  ;; ... other configuration ...
  (global-fci-mode))
```


<a id="org8d3ae79"></a>

## Title abbreviation

The window titles of buffers can get long, and it's nice to abbrevate things like “/home/$USER/” to just “~”. You can do that with the following configuration in `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/user-config ()
  ;; ... other configuration ...
  (extn-spacemacs/setq-default-frame-title-format-enhanced))
```


<a id="org2eefeb6"></a>

## Multiple Xref backend simultaneously

Emacs' Xref support allows multiple backends to be queried, but only one is selected. This layer provides an experimental hack for allowing multiple backends to be used in a cascade.

See the Emacs docstring for `extn-spacemacs/xref-backends-fallingback` for more information.

Enable this feature with the `extn-spacemacs/xref-backends-fallingback` layer configuration in `~/.spacemacs` (after the `:variables` keyword).
