
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Make sure correct version of org-mode is loaded.
(add-to-list 'load-path
             (expand-file-name
              "lisp"
              (expand-file-name "org-mode" site-lisp-dir)))

;; Load everything else from tangled org-file.
(org-babel-load-file
 (expand-file-name "punchagan.org" user-emacs-directory))
