;; Babel setup
(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (ditaa . t)
			       (python . t)
			       (sh . t)))

;; Links
(setq org-return-follows-link t)

;; Org-tree-slide
;; (require 'org-tree-slide)
;; (global-set-key (kbd "<f8>") 'org-tree-slide-mode)

;; Keybindings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)

(provide 'setup-org)
