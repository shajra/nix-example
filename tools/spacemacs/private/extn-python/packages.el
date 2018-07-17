(defconst extn-python-packages
  '(anaconda-mode))

(defun extn-python/post-init-anaconda-mode ()
  (use-package anaconda-mode
    :init
    (remove-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'extn-python/anaconda-mode)
    :config
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "a" 'extn-python/anaconda-mode-restart)))
