;;; -*- lexical-binding: t; -*-

;;; variables

(defcustom +haskell-dante-hlint-when-only 'info
  "Check with hlint only when Dante reporting is not greater than specified.

Note that Flycheck's checkers are chained globally (as symbol properties), so
this variable doesn't work well when set buffer-local.  Set this variable
globally, and it will affect how ‘+haskell/dante-hlint-on’ chains the HLint
checker.

Also see ‘+haskell/dante-hlint-off’, and ‘+haskell/dante-hlint-toggle’."
  :type '(radio
          (const :tag "show HLint when no warnings or errors" info)
          (const :tag "show HLint when no Dante errors" warning)
          (const :tag "show Hlint before Dante any checking" error)
          (const :tag "show Hlint when no Dante reporting (including info)" t))
  :group 'haskell)

(defcustom +haskell-dante-xref-enable t
  "Whether to enable ‘xref’ support for ‘dante-mode’.

Dante's ‘xref’ backend only finds references local to the project. So using a
tool like haskdogs, codex, or nix-haskell-tags, you might be able to set up a
normal etags backend find references in the source of non-local dependencies."
  :type '(radio
          (const :tag "enable Dante Xref backend" t)
          (const :tag "disable Dante Xref backend" nil))
  :group 'haskell)

;;; package configuration

(load! "modules/lang/haskell/+dante.el" doom-emacs-dir)

(add-hook! haskell-literate-mode #'dante-mode)

(after! (dante envrc)
  (defadvice! +haskell--dante-envrc-a (&rest _)
    :before #'dante-start
    (envrc-reload)))

(after! (dante direnv)
  (defadvice! +haskell--dante-direnv-a (&rest _)
    :before #'dante-start
    (direnv-update-directory-environment)))

(after! (dante flycheck)
  (+haskell/dante-hlint-on))

(after! dante
  (+haskell--dante-methods-alist-extend)
  (dolist
      (item
       '("+c"
         "-fdefer-typed-holes"
         "-fdefer-type-errors"
         "-fdiagnostics-color=never"
         "-ferror-spans"
         "-fno-diagnostics-show-caret"
         "-Wall"
         "-Wwarn=missing-home-modules"))
    (add-to-list 'dante-load-flags item))
  (remove-hook 'xref-backend-functions 'dante--xref-backend)
  (defadvice! +haskell--dante-mode-a (orig-fn &rest args)
    :around #'dante-mode
    (+haskell--cond-a 'dante orig-fn args)
    (when +haskell-dante-xref-enable
      (add-hook 'xref-backend-functions 'dante--xref-backend nil t))))
