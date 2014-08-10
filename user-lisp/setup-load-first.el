(require 'cl)
(require 'find-func)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'setup-load-first)
