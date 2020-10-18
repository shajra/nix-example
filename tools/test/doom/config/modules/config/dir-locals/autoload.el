;;; -*- lexical-binding: t; -*-


;;;###autoload
(defun +dir-locals-set-dir-locals (dir mode alist)
  "Set directory local variables for one directory.

Normally we specify directory local variables with “.dir-local.el” files. But
with this function we can specify these variables programmatically in our Emacs
configuration/initialization files. This function wraps
‘dir-locals-set-class-variables’ and ‘dir-locals-set-directory-class’, which are
a touch more verbose to use directly.

DIR is a directory name as a string. MODE is a major mode symbol (use ‘nil’ for
all major-modes). ALIST is an alist associating variables with their settings.

See ‘+dir-locals-set-dirs-locals’ for a function to set variables for multiple
directories at once."
  (let ((class (+dir-locals--class-symbol dir mode)))
    (dir-locals-set-class-variables class `((,mode . ,alist)))
    (dir-locals-set-directory-class dir class)
    (setq-default dir-locals-directory-cache (-distinct dir-locals-directory-cache))))

;;;###autoload
(defun +dir-locals-set-dirs-locals (mode dirlocals &optional baselocals)
  "Set directory local variables for many directories.

We can specify variables for directories individually with
‘+dir-locals-set-dir-locals’, but this function helps more easily specify a base
set of variable settings for multiple directories, allowing for per-directory
overlays.

MODE is a major mode symbol (use ‘nil’ for all major-modes). DIRLOCALS is an
alist associating directory names to the alist associating variables to their
settings. BASELOCALS is an alist suffixed to all the alists in DIRLOCALS."
  (cl-loop for (dir . alist) in dirlocals do
           (+dir-locals-set-dir-locals dir mode (append alist baselocals))))

;;;###autoload
(defun +dir-locals--class-symbol (dir mode)
  (intern (concat dir ":config-dir-locals:" (symbol-name mode))))
