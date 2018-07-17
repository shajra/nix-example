- [Introduction](#org30b60d1)
- [Installation](#org0ad64b0)
- [Features/Configuration](#org8d3ae58)
  - [Global fill-column indicator mode](#org27f0a42)
  - [Title abbreviation](#org62b5ef1)
  - [Multiple Xref backend simultaneously](#org2db9e2b)



<a id="org30b60d1"></a>

# Introduction

This layer extends what's already in Emacs and Spacemacs without bringing in any new packages.

The following features are provided

-   A global fill-column-indicator mode that skips special Emacs buffers
-   A small function to abbreviate titles slightly
-   A hack for enabling multiple Xref backends simultaneously.


<a id="org0ad64b0"></a>

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


<a id="org8d3ae58"></a>

# Features/Configuration


<a id="org27f0a42"></a>

## Global fill-column indicator mode

The `fill-column-indicator` package is already pulled in by Spacemacs, and provides `fci-mode`, which gives a thin margin at your `fill-column` setting. That mode can be turned on per-hooks. This layer offers a global mode that turns on this margin for all buffers execpt for special Emacs buffers with names bracketed by the ‘\*’ character.

You can enable this global mode with the following configuration in `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/user-config ()
  ;; ... other configuration ...
  (global-fci-mode))
```


<a id="org62b5ef1"></a>

## Title abbreviation

The window titles of buffers can get long, and it's nice to abbrevate things like “/home/$USER/” to just “~”. You can do that with the following configuration in `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/user-config ()
  ;; ... other configuration ...
  (extn-spacemacs/setq-default-frame-title-format-enhanced))
```


<a id="org2db9e2b"></a>

## Multiple Xref backend simultaneously

Emacs' Xref support allows multiple backends to be queried, but only one is selected. This layer provides an experimental hack for allowing multiple backends to be used in a cascade.

See the Emacs docstring for `extn-spacemacs/xref-backends-fallingback` for more information.

Enable this feature with the `extn-spacemacs/xref-backends-fallingback` layer configuration in `~/.spacemacs` (after the `:variables` keyword).
