((haskell-mode
  (tags-table-list
   . ("~/src/shajra/nix-package/tutorials/2-haskell/TAGS"
       "~/src/shajra/nix-package/tutorials/2-haskell/TAGS.local"))
  (projectile-tags-backend . xref)
  (projectile-tags-command . "nix-haskell-tags -f ./build.nix -A example-haskell-sourced -e -a")))
