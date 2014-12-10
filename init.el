;;; Code:

;; Set path to dependencies
(setq el-get-dir
      (expand-file-name
       "el-get"
       (expand-file-name "el-get" user-emacs-directory)))

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set path to configs
(setq user-lisp-dir
      (expand-file-name "user-lisp" user-emacs-directory))

;; Setup load path
(add-to-list 'load-path el-get-dir)
(add-to-list 'load-path user-lisp-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Setup packages to load first
(require 'setup-load-first)

;; Setup packages
(require 'setup-el-get)
(require 'setup-package)
(require 'setup-package-list)

;; Install any of my missing packages
(pc/packages-install my-packages)
;; Add any installed packages missing from the list to it.
(pc/add-installed-packages-to-my-packages)

;; Setup appearance
(require 'setup-appearance)
(require 'use-package)

;; Load user specific configuration
(when (file-exists-p user-lisp-dir)
  (mapc 'load (directory-files user-lisp-dir nil "^[^#].*el$")))
