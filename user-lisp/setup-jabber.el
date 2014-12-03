(require 'jabber)

(setq jabber-account-list `(

                            ;; Gmail
                            (,(format "punchagan@gmail.com/Emacs-%s-Jabber-%s" emacs-version jabber-version)
                             (:network-server . "talk.google.com")
                             (:connection-type . starttls))

                            ))

;; Alerts
(setq jabber-alert-presence-hooks nil)
(setq jabber-alert-message-hooks '(jabber-message-scroll jabber-message-notifications))

;;; Enable alerts when focussed out of  emacs
(add-hook 'focus-out-hook
          (lambda () (add-hook 'jabber-alert-message-hooks 'jabber-message-notifications)))

;;; Disable alerts when focussed in emacs
(add-hook 'focus-in-hook
             (lambda () (remove-hook 'jabber-alert-message-hooks 'jabber-message-notifications)))

;; History
(setq
  jabber-history-enabled t
  jabber-use-global-history nil
  jabber-backlog-number 40
  jabber-backlog-days 30)

;; make URLs clickable
(add-hook 'jabber-chat-mode-hook 'goto-address)

(provide 'setup-jabber)
