(require 'mu4e)

;; All maildirs are here in ~/.maildirs but I just use one, as of now.
(setq mu4e-maildir "~/.maildirs/")
;; Multiple accounts can be configured, later
;; See http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html

;; these paths are relative to `mu4e-maildir'
;; set http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html
(setq mu4e-sent-folder   "/muse-amuse/Sent"
      mu4e-drafts-folder "/muse-amuse/Drafts"
      mu4e-trash-folder  "/muse-amuse/Trash")

;; a  list of user's e-mail addresses
(setq mu4e-user-mail-address-list '("punchagan@muse-amuse.in"))

;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:date          .  25)
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  nil)))

;; program to get mail;
(setq
 mu4e-get-mail-command "offlineimap"
 mu4e~get-mail-password-regexp "^Enter password for account '.*?': $"
 ;; Download mail manually.
 mu4e-update-interval nil)

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "punchagan@muse-amuse.in"
      user-mail-address "punchagan@muse-amuse.in"
      user-full-name  "Puneeth Chaganti")

;; smtp mail setting
(setq
   message-send-mail-function 'smtpmail-send-it
   smtpmail-default-smtp-server "muse-amuse.in"
   smtpmail-smtp-server "muse-amuse.in"
   smtpmail-local-domain "muse-amuse.in"

   ;; if you need offline mode, set these -- and create the queue dir
   ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
   smtpmail-queue-mail t
   smtpmail-queue-dir (expand-file-name "queue/cur" mu4e-maildir))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Dealing with html email
(setq mu4e-html2text-command "/home/punchagan/.cabal/bin/pandoc -r html -w plain")

;; Add manual to info
(add-to-list 'Info-directory-list  (file-name-directory (symbol-file 'mu4e-maildir)))

;; Global keybinding for email
(bind-key "<XF86Calculator>" 'mu4e)

;; Enable org-mu4e
;;; org-link creation support
(require 'org-mu4e)
;;; org-mode compose


(provide 'setup-email)
