;;; -*- no-byte-compile: t; -*-


;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! dante
  :recipe (:local-repo "/home/tnks/src/live/dante"))

(package! lsp-mode
  :recipe (:local-repo "/home/tnks/src/live/lsp-mode"))
