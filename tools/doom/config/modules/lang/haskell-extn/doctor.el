;;; -*- lexical-binding: t; -*-


(unless
    (and
     (featurep! :lang haskell)
     (not (featurep! :lang haskell +lsp))
     (not (featurep! :lang haskell +dante)))
  (error! (concat
           "(:lang haskell) is required, but without its +lsp and +dante flags."
           "\n  Use the +lsp/+dante flags of (:lang haskell-extn) instead.")))

(unless
    (or
     (featurep! +lsp)
     (featurep! +dante))
  (warn! (concat
           "You have neither +lsp nor +dante enabled.  At least one of these"
           "\n  features is needed for (:lang haskell-extn) to do anything.")))

(when (featurep! +lsp)
  (unless (featurep! :tools lsp)
    (error! "The +lsp flag of this module requires (:tools lsp).")))

(when (featurep! :tools lsp +eglot)
  (error! "This module only supports lsp-mode, not eglot."))
