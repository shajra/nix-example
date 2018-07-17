(defconst extn-spacemacs-packages
  '((xref :location built-in)))

(defun extn-spacemacs/init-xref ()
  (use-package xref
    :init
    (when extn-spacemacs/xref-backends-fallingback
      (advice-add
       'xref--find-xrefs :override #'extn-spacemacs//find-xrefs-fallingback))))
