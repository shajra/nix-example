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
(defun +haskell-dante-cabal (d)
  "Non-nil iff D has a Cabal file."
  (or
   (directory-files d t ".\\.cabal$")
   (directory-files d t "^package.yaml$")))

;;;###autoload
(defun +haskell-dante-cabal-nix (d)
  "Non-nil iff D has a shell.nix file, and either a Cabal or cabal.project in
the same directory."
  (and (locate-dominating-file d "shell.nix")
       (+haskell-dante-cabal d)))

;;;###autoload
(defun +haskell-dante-cabal-stack-project (d)
  "Non-nil iff D has a Cabal file, with a stack.yaml file also in D or an
ancestor of D."
  (and (locate-dominating-file d "stack.yaml")
       (+haskell-dante-cabal d)))

;;;###autoload
(defun +haskell-dante-cabal-stack-root (d)
  "Non-nil iff D has a Cabal file, with a stack.yaml file also in D or an
ancestor of D."
  (and (or
        (locate-dominating-file (f-dirname (buffer-file-name))
                                (lambda (dir) (directory-files dir t ".\\.cabal$")))
        (locate-dominating-file (buffer-file-name) "package.yaml"))
       (directory-files d t "^stack.yaml$")))

;;;###autoload
(defun +haskell-dante-cabal-stack-project-nix (d)
  "Non-nil iff D has a shell.nix file, and either a Cabal or cabal.project in
the same directory."
  (and (locate-dominating-file d "shell.nix")
       (+haskell-dante-cabal-stack-project d)))

;;;###autoload
(defun +haskell-dante-cabal-stack-root-nix (d)
  "Non-nil iff D has a Cabal file, with a stack.yaml file also in D or an
ancestor of D."
  (and (locate-dominating-file (buffer-file-name) "shell.nix")
       (+haskell-dante-cabal-stack-root d)))

;;;###autoload
(defun +haskell--dante-methods-alist-extend ()
  (setq-default
   dante-methods-alist
   (append
    `(,(+haskell--dante-repl-alt-stack-project-nix-pure)
      ,(+haskell--dante-repl-alt-stack-project-nix-impure)
      ,(+haskell--dante-repl-alt-stack-project)
      ,(+haskell--dante-repl-alt-stack-root-nix-pure)
      ,(+haskell--dante-repl-alt-stack-root-nix-impure)
      ,(+haskell--dante-repl-alt-stack-root)
      ,(+haskell--dante-repl-alt-cabal-nix-pure)
      ,(+haskell--dante-repl-alt-cabal-nix-impure)
      ,(+haskell--dante-repl-alt-cabal))
    dante-methods-alist)))

;;;###autoload
(defun +haskell--dante-repl-alt-cabal ()
  `(alt-cabal
    +haskell-dante-cabal
    ("cabal" "v2-repl"
     (or dante-target nil)
     "--builddir=dist-newstyle/dante"
     "--ghc-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-cabal-nix-pure ()
  `(alt-cabal-nix-pure
    +haskell-dante-cabal-nix
    ("nix-shell" "--pure" (+haskell--find-nix-shell) "--run"
     (concat
      "cabal v2-repl "
      (or dante-target "")
      " --builddir=dist-newstyle/dante"
      " --ghc-options=-ignore-dot-ghci"))))

;;;###autoload
(defun +haskell--dante-repl-alt-cabal-nix-impure ()
  `(alt-cabal-nix-impure
    +haskell-dante-cabal-nix
    ("nix-shell" (+haskell--find-nix-shell) "--run"
     (concat
      "cabal v2-repl "
      (or dante-target "")
      " --builddir=dist-newstyle/dante"
      " --ghc-options=-ignore-dot-ghci"))))

;;;###autoload
(defun +haskell--dante-repl-alt-stack-project ()
  `(alt-stack-project
    +haskell-dante-cabal-stack-project
    ("stack" "repl"
     (or dante-target ".")
     "--ghci-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-stack-root ()
  `(alt-stack-root
    +haskell-dante-cabal-stack-root
    ("stack" "repl"
     (or dante-target nil)
     "--ghci-options=-ignore-dot-ghci")))

;;;###autoload
(defun +haskell--dante-repl-alt-stack-project-nix-pure ()
  `(alt-stack-project-nix-pure
    +haskell-dante-cabal-stack-project-nix
    ("nix-shell" "--pure" (+haskell--find-nix-shell) "--run"
     (concat
      "stack repl "
      (or dante-target ".")
      " --ghc-options=-ignore-dot-ghci"))))

;;;###autoload
(defun +haskell--dante-repl-alt-stack-project-nix-impure ()
  `(alt-stack-project-nix-impure
    +haskell-dante-cabal-stack-project-nix
    ("nix-shell" (+haskell--find-nix-shell) "--run"
     (concat
      "stack repl "
      (or dante-target ".")
      " --ghc-options=-ignore-dot-ghci"))))

;;;###autoload
(defun +haskell--dante-repl-alt-stack-root-nix-pure ()
  `(alt-stack-root-nix-pure
    +haskell-dante-cabal-stack-root-nix
    ("nix-shell" "--pure" (+haskell--find-nix-shell) "--run"
     (concat
      "stack repl "
      (or dante-target "")
      " --ghc-options=-ignore-dot-ghci"))))

;;;###autoload
(defun +haskell--dante-repl-alt-stack-root-nix-impure ()
  `(alt-stack-root-nix-impure
    +haskell-dante-cabal-stack-root-nix
    ("nix-shell" (+haskell--find-nix-shell) "--run"
     (concat
      "stack repl "
      (or dante-target "")
      " --ghc-options=-ignore-dot-ghci"))))

;;;###autoload
(defun +haskell--find-nix-shell ()
  (let ((found (locate-dominating-file (buffer-file-name) "shell.nix")))
    (if found (concat (expand-file-name found) "shell.nix"))))
