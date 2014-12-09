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


;;;; Jabber Message Queue stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'json)

(defconst pc/jabber-message-queue-file
  (expand-file-name ".jabber-message-queue.json" user-emacs-directory)
  "The file where jabber messages are queued")

(defun pc/jabber-add-message-to-queue (to body)
  "Queue up messages when offline, and send on connect."
  (interactive (list
                     (read-string "message: ")))
  (let ((data (append
               (or (ignore-errors (json-read-file pc/jabber-message-queue-file)) '())
               `(((to . ,to) (body . ,body))))))

    (with-temp-buffer
      (insert (json-encode data))
      (write-file pc/jabber-message-queue-file nil))))

(defun pc/jabber-chat-buffer-send ()
  "Send the message in the chat buffer to the queue."
  (interactive)
  (let ((body (delete-and-extract-region jabber-point-insert (point-max))))
    (funcall 'pc/jabber-add-message-to-queue jabber-chatting-with body)))

(defun pc/jabber-chat-with ()
  "Queue messages if not connected, else normal chat."
  (interactive)
  (if (ignore-errors (jabber-read-account))
      (call-interactively 'jabber-chat-with)
    (let ((to (completing-read
               "chat with: "
               ;; yes, that's right! we use mu4e contact list!
               (mapcar (lambda (x) (plist-get x :mail)) mu4e~contact-list))))
      (switch-to-buffer (jabber-chat-create-buffer nil to))
      (local-set-key (kbd "RET") 'pc/jabber-chat-buffer-send))))

(defun pc/jabber-flush-queue (jc)
  "Send all queued messages and empty queue."
  (let ((data (or (ignore-errors (json-read-file pc/jabber-message-queue-file)) '())))
    ;; Send messages
    (mapcar
     (lambda (x) (let ((to (cdr (assoc 'to x)))
                       (body (cdr (assoc 'body x))))
                   (message (format "Sent message to %s: %s" to body))
                   (jabber-send-message jc to nil body "chat")))
     data)

    ;; Delete queue file
    (delete-file pc/jabber-message-queue-file)

    ;; Restore keymap
    (define-key jabber-chat-mode-map (kbd "RET") 'jabber-chat-buffer-send)))

(add-hook 'jabber-post-connect-hooks 'pc/jabber-flush-queue)

;; Bind key to our chat function
(global-set-key (kbd "C-x C-j C-j") 'pc/jabber-chat-with)

(provide 'setup-jabber)
