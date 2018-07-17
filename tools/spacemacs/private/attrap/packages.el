(defconst attrap-packages
  '(attrap))

(defun attrap/init-attrap ()
  (use-package attrap
    :init
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
      "/" 'attrap-attrap)))
