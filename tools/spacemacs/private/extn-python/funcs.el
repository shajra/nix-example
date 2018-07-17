(when (configuration-layer/package-usedp 'anaconda-mode)

  (defun extn-python/anaconda-mode-restart ()
    "‘direnv’-aware restart for ‘anaconda-mode’.

We need to update the Direnv environment before restarting ‘anaconda-mode’,
otherwise we might not pick up the right binaries and Python packages.
"
    (interactive)
    (when (configuration-layer/package-usedp 'direnv)
      (direnv-update-environment))
    (anaconda-mode-stop)
    (anaconda-mode-start))

  (defun extn-python/anaconda-mode ()
    "‘direnv’-aware ‘anaconda-mode’.

We need to update the Direnv environment before starting ‘anaconda-mode’,
otherwise we might not pick up the right binaries and Python packages.
"
    (interactive)
    (when (configuration-layer/package-usedp 'direnv)
      (direnv-update-environment))
    (anaconda-mode)))
