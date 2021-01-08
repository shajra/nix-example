;;; -*- no-byte-compile: t; -*-


;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
;; (package! some-package :recipe (:local-repo "path/to/repo"))


(package! dante :pin "7b32bf21d5b9f7232c4b5c3760abf306e9ed9a0c")
;;(package! dante :recipe
;;  (:host github
;;   :repo "shajra/dante"
;;   :branch "user/shajra/lhs"))

;; TODO: is this needed any more?
;;(package! lsp-mode :recipe
;;  (:host github
;;   :repo "shajra/lsp-mode"
;;   :branch "user/shajra/fix-empty-check"))
