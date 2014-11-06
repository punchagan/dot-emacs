(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "punchagan"
      erc-prompt-for-password t
      erc-prompt ">"
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-query-display 'buffer)

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#emacs-circe" "#emacs-in" "##encamp" "#fossee" "#ipython" "#git" "#github")))

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
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc :server erc-server :port erc-port :nick erc-nick :password (password-read "ERC password: "))))

(provide 'setup-irc)
