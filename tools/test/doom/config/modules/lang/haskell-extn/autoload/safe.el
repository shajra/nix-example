;;; -*- lexical-binding: t; -*-

;;;###autoload
(put '+haskell-backend 'safe-local-variable
     (lambda (v) (-contains? '(dante lsp) v)))

;;;###autoload
(put '+haskell-exclude-regexes 'safe-local-variable #'+haskell--all-regexps?)

;;;###autoload
(put '+haskell-dante-xref-enable 'safe-local-variable #'booleanp)


;;;###autoload
(defun +haskell--all-regexp? (rs)
  (-all? #'haskell--regexp? rs))

;;;###autoload
(defun +haskell--regexp? (r)
  (ignore-errors (string-match-p r "") r))
