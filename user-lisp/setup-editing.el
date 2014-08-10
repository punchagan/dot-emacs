;;; Contains general editing related config.

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; White space
;; Delete trailing white-spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Add new line at end of file
(setq require-final-newline t)

;; Fill column
(setq-default fill-column 79)

;; Highlight matching paren
(show-paren-mode 1)

;; Transperently open compressed files
(auto-compression-mode t)

;; Save a list of recent files
(require 'recentf)
(recentf-mode 1)
