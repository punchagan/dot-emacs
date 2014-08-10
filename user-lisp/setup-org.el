;; Babel setup
(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (ditaa . t)
			       (python . t)
			       (sh . t)))

;; Links
(setq org-return-follows-link t)

;; Keybindings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)

;; Fontify code in blocks
(setq org-src-fontify-natively t)

;; Tabs in src blocks are as if tabs in that mode...
(setq org-src-tab-acts-natively t)

;; Org-tree-slide
(require 'org-tree-slide)
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)

(provide 'setup-org)
