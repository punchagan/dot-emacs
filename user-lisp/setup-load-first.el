(require 'cl)
(require 'dbus)

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'setup-defuns)

(provide 'setup-load-first)
