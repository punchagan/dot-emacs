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
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode 1)

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix nil)

;; Indentation
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; find-file-at-point
(require 'ffap)

;; Save locations in files
(require 'saveplace)
(setq-default save-place t)

;; Alias yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always turn on column numbers
(column-number-mode t)

;; Programming mode generic setup
(add-hook 'prog-mode-hook 'pc/turn-on-line-and-column-numbering)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Seed the random-number generator
(random t)

;; Text mode hooks
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Save visible buffers on focus out
(add-hook 'focus-out-hook 'pc/save-visible-windows)

;; Swap windows
(define-key global-map "\C-cs" 'pc/swap-windows)

;; Dictionary
(define-key global-map "\C-cd" 'dictionary-search)

;; Recursive mini buffers
(setq enable-recursive-minibuffers t)

(provide 'setup-editing)
