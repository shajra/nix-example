(defconst direnv-packages
  '(direnv))

(defun direnv/init-direnv ()
  (use-package direnv
    :config
    (spacemacs/declare-prefix "d" "direnv")
    (spacemacs/set-leader-keys "de" 'direnv-edit)
    (spacemacs/set-leader-keys "dd" 'direnv-update-environment)
    (spacemacs/set-leader-keys "du" 'direnv-update-directory-environment)))
