;; DESIGN: Because Spacemacs writes dynamic customizations to the end of the
;; ~/.spacemacs file, we put our real configuration in another file that we
;; import to keep static and dynamic configuration separate.
;;
(load-file "~/.spacemacs.static.el")
