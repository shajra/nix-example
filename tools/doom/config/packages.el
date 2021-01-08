;;; -*- no-byte-compile: t; -*-


;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
;; (package! some-package :recipe (:local-repo "path/to/repo"))


(package! dante :recipe
  (:host github
   :repo "shajra/dante"
   :branch "user/shajra/lhs"))

;; TODO: is this needed any more?
;;(package! lsp-mode :recipe
;;  (:host github
;;   :repo "shajra/lsp-mode"
;;   :branch "user/shajra/fix-empty-check"))
