;; Circe setup
(require 'circe)

(setq circe-network-options
      `(("Freenode"
         :nick "punchagan"
         :channels ("#emacs" "#emacs-circe" "#emacs-in" "##encamp" "#fossee" "#ipython" "#git" "#github")
         )))

(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose
`circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-server-network)
            (throw 'return t))))))

(defun circe-maybe-connect (network &rest options)
  "Connect to NETWORK, but ask user for confirmation if it's
already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (apply 'circe network options)))

(defun irc ()
  "Connect to IRC"
  (interactive)
  (let ((freenode-password (password-read "Freenode password: ")))
    (circe-maybe-connect "Freenode" :nickserv-password freenode-password)))

(setq lui-flyspell-p t)

;; Circe will hide JOIN, PART and QUIT messages. If a user speaks, Circe will
;; say that this is the first activity of this user, and how long ago they
;; joined. Once they have spoken up like this, Circe will show PART and QUIT
;; normally.
(setq circe-reduce-lurker-spam t)

;; Notifications
(require 'circe-notifications)
(autoload 'enable-circe-notifications "circe-notifications" nil t)
(add-hook 'circe-server-connected-hook 'enable-circe-notifications)
(setq circe-notifications-check-window-focus t)
