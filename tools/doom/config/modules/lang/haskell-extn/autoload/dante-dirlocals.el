;;; -*- lexical-binding: t; -*-
;;;###if (and (featurep! +dante) (featurep! :config dir-locals))


;;;###autoload
(defun +haskell-set-dir-locals-dante (targets &optional baselocals)
  "Set ‘dante-target’ and other directory local variables.

Setting directory-local variables with “.dir-local.el” files can be tedious.
With this function we can set them programmatically.

This function only specifies these variables only for ‘haskell-mode’. See
‘+dir-locals-set-dirs-locals’ for a more general function.

TARGETS is an alist associating directory names to the setting of
‘dante-target’. BASELOCALS is an alist of other directory-local settings to
include for all directories associated in TARGETS."
  (let ((dirlocals
         (cl-loop for (dir . target) in targets collect
                  `(,dir . ((dante-target . ,target))))))
    (+dir-locals-set-dirs-locals 'haskell-mode dirlocals baselocals)))
