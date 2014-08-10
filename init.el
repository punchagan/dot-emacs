;;; Code:

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set path to configs
(setq user-lisp-dir
      (expand-file-name "user-lisp" user-emacs-directory))

;; Setup load path
(add-to-list 'load-path user-lisp-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; Setup packages to load first
(require 'setup-load-first)

;; Setup package
(require 'setup-package)

;; Setup appearance
(require 'setup-appearance)
 
;; Load user specific configuration
(when (file-exists-p user-lisp-dir)
  (mapc 'load (directory-files user-lisp-dir nil "^[^#].*el$")))
