;;; -*- lexical-binding: t; -*-


(load! "modules/lang/haskell/+lsp.el" doom-emacs-dir)

(add-hook! haskell-literate-mode #'lsp!)

(after! (lsp-mode envrc)
  (defadvice! +haskell--lsp-direnv-a (&rest _)
    :before #'lsp
    (envrc-reload)))

(after! (lsp-mode direnv)
  (defadvice! +haskell--lsp-direnv-a (&rest _)
    :before #'lsp
    (direnv-update-directory-environment)))

(after! lsp-mode
  ;; DESIGN: same call as in Doom, but without ":async t".
  ;; After reading the lsp-mode code, I'm pretty sure it's not an async cll.
  (set-lookup-handlers! 'lsp-mode
    :documentation #'lsp-describe-thing-at-point
    :definition #'lsp-find-definition
    :implementations #'lsp-find-implementation
    :type-definition #'lsp-find-type-definition
    :references #'lsp-find-references)
  (defadvice! +haskell--cond-lsp-a (orig-fn &rest args)
    :around #'lsp
    (+haskell--cond-a 'lsp orig-fn args)))
