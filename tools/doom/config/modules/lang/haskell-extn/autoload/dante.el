;;; -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)


;;;###autoload
(defun +haskell/dante-hlint-on ()
  "Add HLint checker after Dante checker."
  (interactive)
  (flycheck-add-next-checker
   'haskell-dante
   `(,+haskell-dante-hlint-when-only . haskell-hlint))
  (message "HLint after Dante checking on"))

;;;###autoload
(defun +haskell/dante-hlint-off ()
  "Remove HLint checker from after Dante checker."
  (interactive)
  (flycheck-remove-next-checker 'haskell-dante 'haskell-hlint)
  (message "HLint after Dante checking off"))

;;;###autoload
(defun +haskell/dante-hlint-toggle ()
  "Toogle on/off HLint checker after Dante checker."
  (interactive)
  (if (-contains? (flycheck-get-next-checkers 'haskell-dante) 'haskell-hlint)
      (+haskell/dante-hlint-off)
    (+haskell/dante-hlint-on)))

;;;###autoload
(defun +haskell-dante-hlint-init-off ()
  "Disable HLint checking after Dante checker upon Doom startup.

The Doom module by default automatically enables HLint checking after Dante
checking.  This function can be called in a user's config.el file to override
this enablement.

Note, due to lazy loading, you can't just call ‘+haskell/dante-hlint-off’ until
the packages (flycheck primarily) have loaded.  Packages are loaded typically
from a haskell-mode hook when you open your first Haskell file.  This function
registers the HLint disablement after packages have loaded."
  (after! (dante flycheck)
    (+haskell/dante-hlint-off)))

;;;###autoload
(defun +haskell-dante-target-guess ()
  "If ROOT is a cabal file, we use it's file name as the guessed target,
which can be overridden with `dante-target'."
  (or dante-target (dante-package-name) nil))

;;;###autoload
(defun +haskell-dante-cabal-nix-alt (d)
  "Non-nil iff D has a shell.nix file, and either a Cabal or cabal.project in
the same directory."
  (and (directory-files d t "^shell\\.nix$")
       (or (directory-files d t "^cabal\\.project$")
           (directory-files d t "\\.cabal$"))))

;;;###autoload
(defun +haskell-dante-cabal-project (d)
  "Non-nil iff D has a Cabal file, with a cabal.project file also in D or an
ancestor of D."
  (and (locate-dominating-file d "cabal.project")
       (directory-files d t "\\.cabal$")))

;;;###autoload
(defun +haskell-dante-cabal-stack (d)
  "Non-nil iff D has a Cabal file, with a stack.yaml file also in D or an
ancestor of D."
  (and (locate-dominating-file d "stack.yaml")
       (directory-files d t "\\.cabal$")))


;;;###autoload
(defun +haskell--dante-methods-alist-extend ()
  (setq-default
   dante-methods-alist
   (append
    `(,(+haskell--dante-repl-alt-stack)
      ,(+haskell--dante-repl-alt-cabal-new-project)
      ,(+haskell--dante-repl-alt-cabal-new-bare)
      ,(+haskell--dante-repl-alt-cabal-v2-project)
      ,(+haskell--dante-repl-alt-cabal-v2-bare)
      ,(+haskell--dante-repl-alt-nix))
    dante-methods-alist)))

;;;###autoload
(defun +haskell--dante-repl-alt-stack ()
  `(alt-stack
    +haskell-dante-cabal-stack
    ("stack" "repl"
     (+haskell-dante-target-guess)
     "--ghci-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-cabal-new-project ()
  `(alt-cabal-new-project
    +haskell-dante-cabal-project
    ("cabal" "new-repl"
     (+haskell-dante-target-guess)
     "--builddir=dist-newstyle/dante"
     "--ghc-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-cabal-new-bare ()
  `(alt-cabal-new-bare
    (directory-files d t "\\.cabal$")
    ("cabal" "new-repl"
     (+haskell-dante-target-guess)
     "--builddir=dist-newstyle/dante"
     "--ghc-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-cabal-v2-project ()
  `(alt-cabal-v2-project
    +haskell-dante-cabal-project
    ("cabal" "v2-repl"
     (+haskell-dante-target-guess)
     "--builddir=dist-newstyle/dante"
     "--ghc-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-cabal-v2-bare ()
  `(alt-cabal-v2-bare
    (directory-files d t "\\.cabal$")
    ("cabal" "v2-repl"
     (+haskell-dante-target-guess)
     "--builddir=dist-newstyle/dante"
     "--ghc-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-nix ()
  `(alt-nix
    +haskell-dante-cabal-nix-alt
    ("nix-shell" "--pure" "--run"
     (concat
      "cabal new-repl "
      (or dante-target "")
      " --builddir=dist-newstyle/dante"
      " --ghc-options=-ignore-dot-ghci"))))
