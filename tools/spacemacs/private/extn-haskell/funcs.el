(when (configuration-layer/package-usedp 'dante)

  ;;; public

  (defun extn-haskell/dante-restart ()
    "‘direnv’-aware replacement for ‘dante-restart’.

We need to update the Direnv environment before restarting Dante, otherwise we
might not pick up the right binaries for Cabal or Ghc.

Also, this function does a flycheck on all relevant buffers.
"
    (interactive)
    (when (configuration-layer/package-usedp 'direnv)
      (direnv-update-environment))
    (dante-restart)
    (when (configuration-layer/package-usedp 'flycheck)
      (let ((cabal-file (dante-cabal-find-file)))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and dante-mode
                       flycheck-mode
                       (equal (dante-cabal-find-file) cabal-file))
              (flycheck-buffer)))))))

  (defun extn-haskell/dante-repl-if-file-upward (root files f)
    "Search ROOT and its parents for a file in FILES and call F with it.

We start with ROOT (typically the Haskell package's root), and test if it
contains a file whose base name is in FILES. If none is found, we try again
with ROOT's parent, until we reach the root of the file system. The return
value is a call to F passing in the first matching filepath. If no matching
file is found, nil is returned.

This is useful for defining functions for a custom function for
‘extn-haskell/dante-repl-types’."
    (cl-some
     (lambda (file)
       (let ((found (extn-haskell//file-search-upward root file)))
         (when found (funcall f found))))
     files))

  (file-name-base "a/b/c.ext")

  (defun extn-haskell/dante-target-guess ()
    "If ROOT is a cabal file, we use it's file name as the guessed target,
which can be overridden with `dante-target'."
    (or dante-target
        (let ((cabal-file (dante-cabal-find-file)))
          (if (equal "cabal" (file-name-extension cabal-file))
              (file-name-base cabal-file)
            nil))))

  ;;; private

  (defun extn-haskell//setq-default-dante-repl (list)
    (setq-default
     dante-repl-command-line-methods-alist (extn-haskell//dante-repl-alist list)
     dante-repl-command-line-methods list))

  (defun extn-haskell//dante-repl-alist (list)
    (let*
        ((alist-old dante-repl-command-line-methods-alist)
         (alist-new (append
                     `(,(extn-haskell//dante-repl-cabal-multi)
                       ,(extn-haskell//dante-repl-stack-multi)
                       ,(extn-haskell//dante-repl-nix-multi)
                       ,(extn-haskell//dante-repl-bare-new))
                     alist-old)))
      (seq-map (lambda (elem) (or (assoc elem alist-new) elem)) list)))

  (defun extn-haskell//dante-repl-bare-new ()
    `(bare-new
      . ,(lambda (root)
           `("cabal" "new-repl"
             ,(extn-haskell/dante-target-guess)
             "--builddir=dist-newstyle/dante"
             "--ghc-options=-ignore-dot-ghci"))))

  (defun extn-haskell//dante-repl-cabal-multi ()
    `(cabal-multi
      . ,(lambda (root)
           (extn-haskell/dante-repl-if-file-upward
            root
            '("cabal.project")
            (lambda (cabal-project)
              `("cabal" "new-repl"
                ,(extn-haskell/dante-target-guess)
                "--builddir=dist-newstyle/dante"
                "--ghc-options=-ignore-dot-ghci"))))))

  (defun extn-haskell//dante-repl-stack-multi ()
    `(stack-multi
      . ,(lambda (root)
           (extn-haskell/dante-repl-if-file-upward
            root
            '("stack.yaml")
            (lambda (stack-yaml)
              `("stack" "repl"
                ,(extn-haskell/dante-target-guess)
                "--ghci-options=-ignore-dot-ghci"))))))

  (defun extn-haskell//dante-repl-nix-multi ()
    `(nix-multi
      . ,(lambda (root)
           (extn-haskell/dante-repl-if-file-upward
            root
            '("shell.nix")
            (lambda (shell-nix)
              `("nix-shell" "--pure" "--run"
                ,(concat
                  "cabal new-repl "
                  (or (extn-haskell/dante-target-guess) "")
                  " --builddir=dist-newstyle/dante"
                  " --ghc-options=-ignore-dot-ghci"
                  )
                ,shell-nix))))))

  (defun extn-haskell//hook-if-not-regex (hook)
    (extn-haskell//hook-regex-guarded '-none? hook))

  (defun extn-haskell//hook-if-regex (hook)
    (extn-haskell//hook-regex-guarded '-any? hook))

  (defun extn-haskell//hook-regex-guarded (g hook)
    (eval
     (lambda ()
       (when
           (and
            (buffer-file-name)
            (funcall g
                     (lambda (regex) (string-match-p regex buffer-file-name))
                     extn-haskell/dante-exclude-regexes))
         (funcall hook)))
     `((g . ,g) (hook . ,hook))))

  (defun extn-haskell//file-search-upward (directory file)
    (let
        ((parent-dir
          (file-truename (concat (file-name-directory directory) "../")))
         (current-path
          (if (not (string= (substring directory (- (length directory) 1)) "/"))
              (concat directory "/" file)
            (concat directory file))))
      (if (file-exists-p current-path)
          current-path
        (when (and
               (not (string= (file-truename directory) parent-dir))
               (< (length parent-dir) (length (file-truename directory))))
          (extn-haskell//file-search-upward parent-dir file))))))
