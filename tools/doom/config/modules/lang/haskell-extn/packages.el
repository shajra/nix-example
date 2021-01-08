;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages.el" doom-emacs-dir)

(package! haskell-mode :pin "0d39c847fddddc5b76fe3c706e34ab45439760bc")

(when (featurep! +dante)
  (package! dante :pin "e2acbf6dd37818cbf479c9c3503d8a59192e34af")
  (package! attrap :pin "9c881548debcf59b8aadda0ef4abca3c9a68dd80"))

(when (and (featurep! +lsp) (not (featurep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "4d85cb3693d893ec34d8a0be9794d468a0a28b7b"))
