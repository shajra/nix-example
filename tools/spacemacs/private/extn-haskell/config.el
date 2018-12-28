(defvar extn-haskell/dante-exclude-regexes
  '("/\\.haskdogs/" "/\\.codex/" "/\\.stack/" "/^\/run\/user\//")
  "Regular expressions to exclude matching buffers from ‘dante-mode’.

Source code doesn't always have well-formed project files, which prevents Dante
from loading. This happens in particular with source downloaded with tools like
Haskell's haskdogs and codex.")


(defvar extn-haskell/dante-flycheck-hlint-enable nil
  "Whether ‘dante-mode’ should include an hlint checker. This presumes you have
hlint on your path.")


(defvar extn-haskell/dante-flycheck-hlint-level 'warning
  "The ‘flycheck’ level to use is the hlint checker is enabled. See
‘extn-haskell/dante-flycheck-hlint-enable’. Allowed values are ‘warning’,
‘error’, and ‘info’.")


(defvar extn-haskell/dante-load-flags-extra '("-Wall")
  "Extra flags added on top of (not replacing) ‘dante-load-flags’.

See URL `https://github.com/jyp/dante/issues/54' for a discussion of why you
may want to try out \"-fdefer-typed-holes\" or \"-fdefer-type-errors\".")


(defvar extn-haskell/dante-repl-types
  '(nix stack styx mafia new-build bare)
  "Priority order of ways to try to run GHCi for ‘dante-mode’.

This improves upon setting ‘dante-repl-command-line-methods-alist’ directly.

Allowed values include a set of symbols (described below) or custom function.

Provided functions should accept as a parameter the detected Haskell package's
root (see ‘dante-project-root’). The function should return the command line
invocation (as a list of strings) for a GHCi REPL for the package.

So you don't have to write as many functions, you can alternatively use symbols
as aliases for built-in functions.

The following symbols alias functions from the default setting of
‘dante-repl-command-line-methods-alist’:

    ‘bare’: run cabal repl unconditionally

    ‘mafia’: run mafia if a \"mafia\" file is in the root

    ‘new-build’: run cabal new-repl if a \"*.cabal\" or a \"cabal.project\"
    file is in the root

    ‘nix’: run cabal repl in a nix-shell if a \"shell.nix\" or \"default.nix\"
    file is in the root

    ‘stack’: run stack repl if a \"stack.yaml\" file is in the root

    ‘styx’: run styx repl if a \"sytx.yaml\" file is in the root

There's a few shortages with the options above. Firstly, they invoke a GHCi
session without ignoring the user-level .ghci file, which can sometimes
conflict with project-level compilation. Also, because Cabal and Stack support
multiple packages, a \"cabal.project\" or \"stack.yaml\" file might not just be
in the package root, but in one of its ancestors.

The follow symbols alias additional built-in functions address these problem:

    ‘bare-new’: like ‘bare’, but call cabal new-repl

    ‘cabal-multi’: run cabal new-repl if a \"cabal.project\" file is found in an
    ancestor of the root

    ‘nix-multi’: run cabal new-repl in a nix-shell if a \"shell.nix\" file is
    found in an ancestor of the root.

    ‘stack-multi’: run stack repl in a nix-shell if a \"stack.yaml\" file is
    found in an ancestor of the root.")


(defvar extn-haskell/dante-xref-enable t
  "Whether to enable ‘xref’ support for ‘dante-mode’.

Dante's ‘xref’ backend only finds references local to the project. So using
using a tool like Haskell's haskdogs or codex, you might be able to set up a
normal etags backend find references in source to non-local dependencies.")
