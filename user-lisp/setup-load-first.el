(require 'cl)
(require 'bind-key)
(require 'use-package)

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'setup-defuns)

(provide 'setup-load-first)
