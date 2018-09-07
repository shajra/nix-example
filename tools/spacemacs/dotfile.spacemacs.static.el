(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(;; Refresh with <SPC f e R>

     ;; lang
     bibtex
     c-c++
     emacs-lisp
     graphviz
     html
     java
     latex
     markdown
     python
     scala
     shell-scripts
     sql
     yaml

     ;; tools
     docker
     pandoc
     restclient
     shell
     systemd

     ;; other
     auto-completion
     git
     github
     gtags
     helm
     nixos
     (org :variables org-enable-github-support t)
     spell-checking
     syntax-checking
     themes-megapack
     version-control
     (vinegar :variables vinegar-reuse-dired-buffer t)

     ;; private
     attrap
     direnv
     (extn-haskell
      :variables
      extn-haskell/dante-flycheck-hlint-enable t
      extn-haskell/dante-repl-types '(cabal-multi stack-multi bare-new)
      extn-haskell/dante-xref-enable nil  ; use nix-tags-haskell instead
      )
     extn-python
     extn-spacemacs)
   dotspacemacs-additional-packages
   '(auto-package-update column-enforce-mode helm-xref)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used))


(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5) (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-light solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '(("Source Code Pro 12"))
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))


(defun dotspacemacs/user-init ()
  (setq-default
   fci-rule-color "#93a1a1"))


(defun dotspacemacs/user-config ()

  ;; simple settings
  (setq-default
   auto-package-update-delete-old-versions t
   auto-package-update-prompt-before-update t
   ensime-startup-notification nil
   face-remapping-alist '((helm-xref-file-name :foreground "#2aa198"))
   fill-column 79
   flycheck-check-syntax-automatically '(mode-enabled save)
   haskell-indent-spaces 4
   haskell-process-auto-import-loaded-modules t
   haskell-compile-cabal-build-command "cd %s && cabal new-build --ghc-option=-ferror-spans"
   haskell-cabal-commands '("new-build"
                            "new-bench"
                            "check"
                            "clean"
                            "new-configure"
                            "copy"
                            "fetch"
                            "new-haddock"
                            "help"
                            "hscolour"
                            "info"
                            "init"
                            "new-install"
                            "list"
                            "register"
                            "report"
                            "new-run"
                            "sdist"
                            "new-test"
                            "unpack"
                            "new-update"
                            "upgrade"
                            "upload")
   haskell-process-log t
   haskell-process-suggest-remove-import-lines t
   haskell-process-type 'cabal-new-repl
   haskell-stylish-on-save t
   helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long
   indent-tabs-mode nil
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   tab-width 4
   whitespace-line-column 79
   x-gtk-use-system-tooltips nil
   xref-show-xrefs-function 'helm-xref-show-xrefs)

  ;; abbreviate title
  (extn-spacemacs/setq-default-frame-title-format-enhanced)

  ;; enable non-layer packages
  (auto-package-update-maybe)
  (global-column-enforce-mode)
  (global-fci-mode)
  (require 'helm-xref)

  ;; safe local settings
  (dolist
      (item
       '((projectile-tags-backend . xref)
         (projectile-tags-command . "nix-tags-haskell -e")))
    (add-to-list 'safe-local-variable-values item))

  ;; hacks
  ;;
  ;; Linux Libertine doesn't render Unicode diminished-mode characters well
  (add-to-list 'face-ignored-fonts "Linux Libertine")
  ;;
  ;; the dialog verifying a large tag table breaks automated selection
  (add-to-list 'spacemacs-large-file-modes-list 'tags-table-mode))
