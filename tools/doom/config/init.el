;;; -*- lexical-binding: t; -*-


(doom!

 :completion
 company
 helm
 ivy  ; or ido

 :ui
 doom
 doom-dashboard
 doom-quit
 fill-column
 hl-todo
 hydra
 indent-guides
 modeline
 nav-flash
 ophints
 (popup
  +all
  +defaults)
 ;;tabs  ; tried, but not sure I like it
 treemacs  ; or neotree
 unicode
 vc-gutter
 vi-tilde-fringe
 window-select
 ;workspaces

 :editor
 (evil +everywhere)
 file-templates
 fold
 format
 multiple-cursors
 rotate-text
 snippets
 word-wrap

 :checkers
 (spell +aspell)
 syntax

 :emacs
 dired
 electric
 ibuffer
 undo
 vc

 :term
 vterm

 :tools
 ;;direnv
 emacs-direnv
 docker
 (eval +overlay)
 gist
 (lookup
  +docsets)
 (lsp
  +peek)
 ;;macos
 magit
 make
 rgb

 :os  ; macos, tty

 :lang
 ;;cc
 data
 emacs-lisp
 haskell
 (haskell-extn +dante +lsp)
 latex
 markdown
 nix
 (org
  ;;+pandoc
  ;;+present
  )
 ;;plantuml
 (python
  +lsp
  +pyright)
 rest
 sh
 ;;web
 yaml

 :email  ; mu4e, notmuch, wanderlust

 :app  ; calendar, irc, rss, twitter

 :config
 (default +bindings +smartparens)
 dir-locals
 )
