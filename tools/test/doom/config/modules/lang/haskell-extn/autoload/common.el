;;; -*- lexical-binding: t; -*-
;;;###if (or (featurep! +lsp) (featurep! +dante))


;;;###autoload
(defun +haskell--flycheck-checkers-disable-h ()
  (when (or (eq +haskell-backend 'dante) (eq +haskell-backend 'lsp))
    (dolist (item '(haskell-ghc haskell-stack-ghc))
      (flycheck-disable-checker item)))
  (when (not (eq +haskell-backend 'dante))
    (flycheck-disable-checker 'haskell-dante))
  (when (not (eq +haskell-backend 'lsp))
    (flycheck-disable-checker 'lsp)))

;;;###autoload
(defun +haskell--checkers-disable-h ()
  (add-hook! flycheck-mode #'+haskell--flycheck-checkers-disable-h))

;;;###autoload
(defun +haskell--cond-a (backend orig-fn &rest args)
  (when (and
       (eq +haskell-backend backend)
       (not (-any?
             (lambda (re) (string-match-p re buffer-file-name))
             +haskell-exclude-regexes)))
      (apply orig-fn args)))
