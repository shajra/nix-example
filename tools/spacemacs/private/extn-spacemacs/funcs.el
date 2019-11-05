;;; public

(when (configuration-layer/package-usedp 'fill-column-indicator)
  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1)))))

(defun extn-spacemacs/set-dir-locals (dir mode alist)
  "Set directory local variables for one directory.

Normally we specify directory local variables with “.dir-local.el” files. But
with this function we can specify these variables programmatically in our Emacs
configuration/initialization files. This function wraps
‘dir-locals-set-class-variables’ and ‘dir-locals-set-directory-class’, which
are a touch more verbose to use directly.

DIR is a directory name as a string. MODE is a major mode symbol (use ‘nil’ for
all major-modes). ALIST is an alist associating variables with their settings.

See ‘extn-spacemacs/set-dirs-locals’ for a function to set variables for
multiple directories at once."
  (let ((class (extn-spacemacs//class-symbol dir mode)))
    (dir-locals-set-class-variables class `((,mode . ,alist)))
    (dir-locals-set-directory-class dir class)
    (setq-default dir-locals-directory-cache (-distinct dir-locals-directory-cache))))

(defun extn-spacemacs/set-dirs-locals (mode dirlocals &optional baselocals)
  "Set directory local variables for many directories.

We can specify variables for directories individually with
‘extn-spacemacs/set-dir-locals’, but this function helps more easily specify a
base set of variable settings for multiple directories, allowing for
per-directory overlays.

MODE is a major mode symbol (use ‘nil’ for all major-modes). DIRLOCALS is an
alist associating directory names to the alist associating variables to their
settings. BASELOCALS is an alist suffixed to all the alists in DIRLOCALS."
  (cl-loop for (dir . alist) in dirlocals do
           (extn-spacemacs/set-dir-locals dir mode (append alist baselocals))))

;;; private

(defun extn-spacemacs//find-xrefs-fallingback (input kind id display-action)
  (let ((fn (intern (format "xref-backend-%s" kind)))
        (tail xref-backend-functions))
    (cl-block nil
      (dolist (backend-fn xref-backend-functions)
        (let ((backend (and (symbol-function backend-fn) (funcall backend-fn))))
          (when backend
            (let ((xrefs (funcall fn backend id)))
              (when xrefs
                (cl-return (xref--show-xrefs xrefs display-action))))
            ;; DESIGN: this is where the API for Xref breaks down with respect
            ;; to this strategy of falling back on multiple backends. The
            ;; identifier is calculated for a particular backend. That
            ;; identifier might work with other backends (above), but it also
            ;; might need to be recalculated and retried (below).
            (let* ((id-recalc (xref-backend-identifier-at-point backend))
                   (xrefs (and id-recalc (funcall fn backend id-recalc))))
              (when xrefs
                (cl-return (xref--show-xrefs xrefs display-action)))))))
      (user-error "No %s found for: %s" (symbol-name kind) input))))

(defun extn-spacemacs//class-symbol (dir mode)
  (intern (concat dir ":spacemacs-extn:" (symbol-name mode))))
