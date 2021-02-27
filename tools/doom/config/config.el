;;; -*- lexical-binding: t; -*-


;; Simple settings

(setq-default
 dante-methods '(alt-stack-project alt-cabal)
 doom-theme 'doom-solarized-light
 fancy-splash-image "~/.config/doom/snowman.png"
 fill-column 80
 +haskell-dante-xref-enable nil
 haskell-hoogle-command nil
 lsp-enable-xref nil
 lsp-haskell-formatting-provider "stylish-haskell"
 lsp-haskell-server-path "haskell-language-server-wrapper"
 org-src-preserve-indentation t
 whitespace-line-column 79)


;; Function calls

(doom/set-indent-width 4)
(global-display-fill-column-indicator-mode)
(after! treemacs (treemacs-follow-mode))


;; Hooks

(after! xref
  (add-hook 'xref-backend-functions 'etags--xref-backend))

;; DESIGN: doom dashboard looks bad with an indicator
(add-hook! +doom-dashboard-mode :append
  (display-fill-column-indicator-mode -1))

;; DESIGN: preferred Spacemacs' default formatting (also don't want to change
;; everything)
(add-hook! org-load :append
  (setq
   org-startup-indented nil))

;; DESIGN: Some configuration is more personal...
(load! "private/config")
