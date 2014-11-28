(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)
(require 'erc-desktop-notifications)

(require 'tls)
(setq tls-program '("gnutls-cli --priority secure256 -p %p %h"))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

(setq erc-prompt-for-password t
      erc-prompt (lambda () (concat "[" (buffer-name) "]"))
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-query-display 'buffer
      erc-notifications-icon notifications-application-icon)

(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#emacs" "#org-mode" "#emacs-in" "#fossee" "#ipython" "#git" "#github"
         "#scikit-learn" "#scikit-image" "#nikola")))

;; track
(erc-track-mode t)
(setq erc-track-enable-keybindings t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; Logging
(setq erc-log-channels-directory "~/.erc/logs/")
(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))
(setq erc-save-buffer-on-part t)

;; enable spell checking
(erc-spelling-mode 1)

;; autoaway setup
(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 600)
(setq erc-autoaway-use-emacs-idle t)

;; notifications
(add-to-list 'erc-modules 'notifications)
(erc-notifications-mode)
(add-hook 'focus-out-hook 'erc-notifications-enable)
(add-hook 'focus-in-hook 'erc-notifications-disable)

;; /SLAP command
(defun erc-cmd-SLAP (&rest nick)
  (erc-send-action
   (erc-default-target)
   (concat "slaps " (car nick) " around the solar system "
           "-- just out of spite!")))

;; disable yasnippet
(add-hook 'erc-mode-hook (lambda () (yas-minor-mode -1)))

;; start and stop commands
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "punchagan" :password (password-read "Freenode password: "))
  (erc-tls :server "kanjar.irc.slack.com" :port 6667 :nick "punchagan" :password (cadr (auth-source-user-and-password "kanjar"))))

(provide 'setup-irc)
