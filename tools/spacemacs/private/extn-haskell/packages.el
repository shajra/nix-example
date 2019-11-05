(defconst extn-haskell-packages
  '(attrap
    dante
    flycheck
    haskell-mode))

(defun extn-haskell/pre-init-attrap ()
  (spacemacs|use-package-add-hook attrap
    :post-config
    (dolist (mode haskell-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "r/"  'attrap-attrap))))

(defun extn-haskell/pre-init-dante ()
  (spacemacs|use-package-add-hook dante
    :post-init
    (if (configuration-layer/package-usedp 'direnv)
        (spacemacs/add-to-hooks
         (extn-haskell//hook-if-not-regex
          (lambda () (direnv-update-environment) (dante-mode)))
         (extn-haskell//mode-hooks))
      (spacemacs/add-to-hooks
       (extn-haskell//hook-if-not-regex 'dante-mode)
       (extn-haskell//mode-hooks)))
    :post-config
    (spacemacs|diminish dante-mode "Ⓓ" "D")
    (dolist (mode haskell-modes)
      (spacemacs/declare-prefix-for-mode mode
        "m," "haskell/dante")
      (spacemacs/set-leader-keys-for-major-mode mode
        ",\"" 'dante-eval-block
        ",."  'dante-info
        ",,"  'dante-type-at
        ",i"  'spacemacs-haskell//dante-insert-type
        ",r"  'extn-haskell/dante-restart
        "sr"  'extn-haskell/dante-restart
        ",d"  'dante-diagnose))
    (when (not extn-haskell/dante-xref-enable)
      (remove-hook 'xref-backend-functions 'dante--xref-backend))
    (extn-haskell//setq-default-dante-repl extn-haskell/dante-repl-types)
    (dolist (flag extn-haskell/dante-load-flags-extra)
      (add-to-list 'dante-load-flags flag))))

(defun extn-haskell/pre-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    :post-init
    (when (configuration-layer/package-usedp 'haskell-mode)
      (dolist (mode haskell-modes) (spacemacs/enable-flycheck mode))
      (when extn-haskell/dante-flycheck-hlint-enable
        (add-hook
         'dante-mode-hook
         (lambda ()
           (flycheck-add-next-checker
            'haskell-dante
            `(,extn-haskell/dante-flycheck-hlint-level . haskell-hlint))))))))

(defun extn-haskell/pre-init-haskell-mode ()
  (spacemacs|use-package-add-hook haskell-mode
    :post-init
    ;; DESIGN: Fixes a bug in which haskell-mode tries to run stylish-haskell
    ;; on literate Haskell files (not supported) if haskell-stylish-on-save is
    ;; enabled.
    (add-hook 'literate-haskell-mode-local-vars-hook
              (lambda ()
                (remove-hook
                 'before-save-hook
                 'haskell-mode-before-save-handler
                 t))
              t)
    ;; DESIGN: removing setup from spacemacs-haskell layer
    (dolist (hook (extn-haskell//mode-hooks))
      (remove-hook hook 'spacemacs-haskell//setup-backend))))
