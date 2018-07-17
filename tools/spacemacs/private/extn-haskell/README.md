- [Introduction](#orge2bfef3)
- [Installation](#orgadae9c7)
  - [Dependencies](#orgbac4126)
  - [Enabling the layer](#org293906f)
- [Features/Configuration](#org98d0e98)
  - [Layer configuration](#orgaafbc89)
  - [Key bindings](#org5c4b6da)
    - [From the `haskell` Spacemacs layer](#org844c7cc)
    - [From this layer](#org1f3168e)



<a id="orge2bfef3"></a>

# Introduction

This layer extends the Spacemacs-provided `haskell` layer, but fully commits to integration with [Dante](https://github.com/jyp/dante) disabling competing/redundant support for [ghc-mod](http://hackage.haskell.org/package/ghc-mod), and [Intero](https://commercialhaskell.github.io/intero/). Although Dante has less support for some features like completion and navigating references it has some benefits:

-   Tools like Intero obligate projects to work with Stack. This makes integration with projects not using Stack (say using only Cabal or Nix) more tedious. Dante still supports Stack, but optionally if a `stack.yaml` is detected.

-   Dante delegates much more to GHC/GHCi, and thus has a much easier time keeping up with later releases of GHC.

Here's a summary of the features provided by this layer:

-   major-mode key bindings and hooks to enable Dante for Haskell files layering on top of the key bindings already provided by the Spacemacs `haskell` layer

-   simplified layer-based configuration of Dante including:
    -   easier configuration of Dante project types and detection
    
    -   Flycheck Hlint integration
    
    -   control to enable/disable Dante's Xref backend
    
    -   exclusion of Dante from buffers matching a regular expression
    
    -   GHC flags for the GHCi sessions Dante spawns

-   [emacs-direnv](https://github.com/wbolster/emacs-direnv)/[Direnv](https://direnv.net/) integration for per-project discovery of Cabal/GHC binaries.

Some features come from the Spacemacs `haskell` layer this layer extends such including:

-   syntax highlighting (as one would expect)
-   support for literate Haskell files (bird-track and LaTeX styles)
-   a package/dependency-loaded GHCi interactive buffer
-   HLint refactoring/fixing
-   Hoogle/Hayoo documentation lookup
-   [Stylish-Haskell](https://github.com/jaspervdj/stylish-haskell) formatting

Finally, here's a few things that are lacking that hopefully might improve in the future:

-   [Dante's completion support is lacking](https://github.com/jyp/dante/issues/54)
-   [Dante's Xref support is finicky](https://github.com/jyp/dante/issues/78)
-   Emacs-based debugging support is fragile/broken, especially with newer GHC versions


<a id="orgadae9c7"></a>

# Installation


<a id="orgbac4126"></a>

## Dependencies

Beyond the normal build tools for a Haskell project (GHC, Cabal, Stack, etc.) to use this layer fully you may want the following executables (all are Haskell application distributed on Hackage):

-   `apply-refact` (required by `hlint-refactor`)
-   `hlint` (required by `hlint-refactor`)
-   `stylish-haskell` (optional for `haskell-mode`)
-   `hasktags` (optional for Xref)
-   `hoogle` (optional for `haskell-mode` and `helm-hoogle`)

You can install these dependencies using Cabal, Stack, or an OS package manager like Nix. Make sure they are on your `PATH`. Consider using a tool like Direnv to manage all your environment variables at the directory-level, including `PATH`. You can even use the `direnv` Spacemacs layer distributed along with this layer.


<a id="org293906f"></a>

## Enabling the layer

To use this configuration layer, copy or link this layer's `extn-haskell` directory to the private layers directory of your Spacemacs installation keeping the name (`~/.emacs.d/private/extn-haskell`).

Then enable it in your `~/.spacemacs`:

```emacs-lisp
(defun dotspacemacs/layers ()
  (setq-default
     ;; ... other configuration ...
   dotspacemacs-configuration-layers
     '( ; ... other layers
       (extn-haskell :variables
	;; layer settings in pairs; for example...
	extn-haskell/dante-flycheck-hlint-enable t
	extn-haskell/dante-repl-types '(cabal-multi stack-multi bare-new)
	extn-haskell/dante-xref-enable nil))))
```


<a id="org98d0e98"></a>

# Features/Configuration


<a id="orgaafbc89"></a>

## Layer configuration

The follow layer settings are supported with the `:variables` keyword when enabling the layer:

-   `extn-haskell/dante-exclude-regexes`
-   `extn-haskell/dante-flycheck-hlint-enable`
-   `extn-haskell/dante-flycheck-hlint-level`
-   `extn-haskell/dante-load-flags-extra`
-   `extn-haskell/dante-repl-types`
-   `extn-haskell/dante-xref-enable`

See [their definitions/docstrings](./config.el) for more details on these options.


<a id="org5c4b6da"></a>

## Key bindings


<a id="org844c7cc"></a>

### From the `haskell` Spacemacs layer

Many keys bind from the ~haskell= layer. Rather than redocument those bindings here, see the [documentation for that layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/haskell#key-bindings). Note that because this layer disables Intero and ghc-mod support, key bindings for those won't be available.


<a id="org1f3168e"></a>

### From this layer

Dante-specific commands are prefixed by `SPC m ,`

| Key Binding | Description                                           |
|----------- |----------------------------------------------------- |
| `SPC m , "` | evaluate comment line of the form “&#x2013; >>> expr” |
| `SPC m , .` | GHCi :info for the expression at the point            |
| `SPC m , ,` | GHCi :type for the expression at the point            |
| `SPC m , r` | restart Dante GHCi process                            |
| `SPC m , d` | diagnose Dante in `*Help*` buffer                     |

See the [Dante documentation](https://github.com/jyp/dante) for more information.

Additionally, since some Dante repairs are supported by the [Attrap Emacs package](https://github.com/jyp/attrap) the follow key binding is made available:

| Key Binding | Description                                     |
|----------- |----------------------------------------------- |
| `SPC m r /` | attempt to repair a Haskell defect at the point |

You can enable Attrap with the `attrap` layer provided by this project.
