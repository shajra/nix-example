;;; -*- lexical-binding: t; -*-


(when (or (featurep! +lsp) (featurep! +dante))

  (defcustom +haskell-backend
    (cond
     ((featurep! +lsp) 'lsp)
     ((featurep! +dante) 'dante))
    "Backend to use for Haskell support.

There's a variety of packages that support Haskell for Emacs.  There has always
been ‘haskell-mode’ which a solid base of features (see
http://haskell.github.io/haskell-mode/manual/latest/ for a full list).

However, for all the great things ‘haskell-mode’ has, it's missing features
people may expect from modern IDEs, for instance good interactive feedback of
errors.  You can use ‘flycheck’ for this, which does have some Haskell support,
however, this does a complete build to get compilation errors, which can be slow
on large projects.

To get faster feedback with incremental builds we have two options:

- ‘dante-mode’ (https://github.com/jyp/dante)
- ‘lsp-mode’ (https://github.com/jyp/dante)
- Language Server Protocol (LSP) support for Haskell"
    :type '(radio
            (const :tag "Dante" dante)
            (const :tag "LSP" lsp))
    :group 'haskell)

  (defcustom +haskell-exclude-regexes
    '("/\\.haskdogs/"
      "/\\.codex/"
      "/\\.stack/"
      "/\\.stack-work/"
      "^/run/user/"
      "^/nix/store/")
    "Regular expressions to exclude matching buffers from ‘dante-mode’.

Source code doesn't always have well-formed project files, which prevents Dante
from loading. This happens in particular with source downloaded with tools like
Haskell's haskdogs and codex."
    :type '(set regexp)
    :group 'haskell)

  (after! (haskell-mode flycheck)
    (add-hook! haskell-mode-local-vars #'+haskell--checkers-disable-h)
    (add-hook! haskell-literate-mode #'+haskell--checkers-disable-h)))

(when (featurep! +dante)
  (load! "+dante"))

(when (featurep! +lsp)
  (load! "+lsp"))
