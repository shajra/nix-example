;;; -*- lexical-binding: t; -*-


;;;###autoload
(put 'projectile-tags-backend 'safe-local-variable
     (lambda (l) (-contains? '(auto xref ggtags etags-select find-tag) l)))

;;;###autoload
(put 'projectile-tags-command 'safe-local-variable
     (lambda (v) (s-starts-with? "nix-haskell-tags" v)))

;;;###autoload
(put 'tags-table-list 'safe-local-variable
     (lambda (l) (-all? #'stringp l)))
