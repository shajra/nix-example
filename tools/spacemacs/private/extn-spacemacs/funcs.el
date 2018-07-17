;;; public

(defun extn-spacemacs/setq-default-frame-title-format-enhanced ()
  "Set a ‘frame-title-format’ that abbreviates file names. The main
abbreviation is turning \"/home/username/\" to just \"~\".

It's not much of an enhancement for now, but it could be extended later."
  (setq-default
   frame-title-format (extn-spacemacs//frame-title-format-enhanced-eval)))

(when (configuration-layer/package-usedp 'fill-column-indicator)
  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1)))))

;;; private

(defun extn-spacemacs//frame-title-format-enhanced-eval ()
  '((:eval (extn-spacemacs//frame-title-format-enhanced))))

(defun extn-spacemacs//frame-title-format-enhanced ()
  (if (buffer-file-name)
      (abbreviate-file-name (buffer-file-name))
    "%b"))

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
