;;; -*- lexical-binding: t; -*-


;; Simple settings

(setq-default
 dante-methods '(alt-cabal-new-project alt-stack alt-cabal-new-bare alt-nix)
 doom-theme 'doom-solarized-light
 fancy-splash-image "~/.config/doom/snowman.png"
 fill-column 80
 +haskell-dante-xref-enable nil
 haskell-hoogle-command nil
 lsp-enable-xref nil
 lsp-haskell-process-path-hie "haskell-language-server-wrapper"
 whitespace-line-column 79
 +workspaces-switch-project-function (lambda (_) (projectile-commander)))

;; Function calls

(doom/set-indent-width 4)
(global-display-fill-column-indicator-mode)
(after! treemacs (treemacs-follow-mode))

(after! projectile
  (setq-default projectile-commander-methods ())
  (def-projectile-commander-method ?f
    "Find file in project."
    (counsel-projectile-find-file))
  (def-projectile-commander-method ?d
    "Find directory in project."
    (counsel-projectile-find-dir))
  (def-projectile-commander-method ?D
    "Open project root in dired."
    (projectile-dired))
  (def-projectile-commander-method ?s
    "Run ripgrep on project."
    (counsel-projectile-rg))
  (def-projectile-commander-method ?v
    "Open project root in vc-dir or magit."
    (projectile-vc))
  (def-projectile-commander-method ?t
    "Find tag in project."
    (projectile-find-tag)))

;; Hooks

(after! xref
  (add-hook 'xref-backend-functions 'etags--xref-backend))

(add-hook! +doom-dashboard-mode :append
  (display-fill-column-indicator-mode -1))

;; DESIGN: preferred Spacemacs' default formatting (also don't want to change
;; everything)
(add-hook! org-load :append
  (setq
   org-startup-indented nil
   org-src-preserve-indentation nil))

;; DESIGN: electric-indent-mode seems not to work well with Org-mode:
;;     http://www.foldl.me/2012/disabling-electric-indent-mode/
;;     https://www.philnewton.net/blog/electric-indent-with-org-mode/
;;
;; But it might be fixed with an upcoming patch:
;;     https://orgmode.org/list/877dxpazbo.fsf_-_@gmail.com/
;;
;; I don't think this is a problem anymore, so I've disabled the hook.
;;
(add-hook! org-mode
   (set (make-local-variable 'electric-indent-functions)
        (list (lambda (_arg) 'no-indent))))

;; DESIGN: Some configuration is more personal...
(load! "private/config")
