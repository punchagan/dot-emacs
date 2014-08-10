(require 'cl)
;; (require 'saveplace)
;; (require 'ffap)
;; (require 'uniquify)
;; (require 'ansi-color)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'setup-load-first)
