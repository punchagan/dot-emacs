;;; init.el --- Where all the magic begins
;;
;; A shameless copy & adaption of the Emacs Starter Kit by eschulte
;;
;; This is the first thing to get loaded.
;;

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org" (expand-file-name
                                        "elisp" dotfiles-dir))))
;; Load up Org Mode and Babel
(require 'org-install)

;; load up the main file
(org-babel-load-file (expand-file-name "emacs-kit.org" dotfiles-dir))

;;; init.el ends here
