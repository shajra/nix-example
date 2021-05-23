;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages.el" doom-emacs-dir)

(package! haskell-mode :pin "1baa12abfa2c81128e5b13d1351f2978a4a54b4f")

(when (featurep! +dante)
  (package! dante :pin "8741419333fb85ed2c1d71f5902688f5201b0a40")
  (package! attrap :pin "a5bc695af27349ae6fe4541a581e6fd449d2a026"))

(when (and (featurep! +lsp) (not (featurep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "7efbef3d206989faa8b691a4230a3ed872542187"))
