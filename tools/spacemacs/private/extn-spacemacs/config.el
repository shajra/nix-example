(defvar extn-spacemacs/xref-backends-fallingback nil
  "Have ‘xref’ fallback through the list of apropos backends.

‘xref-backend-functions’ does have multiple functions requested in order to
find a backend, but only the first one found is used. That backend may not
actually find a reference. This happens occasionally when a backend only covers
a fraction of all the references.

This setting enables a hack to allow all backends to be tried in the order they
are found via ‘xref-backend-functions’. It's still an experimental, and the
algorithm is definitely a hack due to the API of ‘xref’ not really being
designed for this.")
