;;; -*- lexical-binding: t; -*-


(use-package! direnv
  :when (executable-find "direnv")
  :after-call doom-first-file-hook
  :config
  (direnv-mode))
