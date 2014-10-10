;; Org-directory
(setq org-directory "~/.life-in-plain-text/")

;; Babel setup
(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (ditaa . t)
			       (python . t)
			       (sh . t)))

;; Links
(setq org-return-follows-link t)

;; Keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Fontify code in blocks
(setq org-src-fontify-natively t)

;; Tabs in src blocks are as if tabs in that mode...
(setq org-src-tab-acts-natively t)

;; Org-tree-slide
(require 'org-tree-slide)
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)

;; Capture related stuff
(require 'org-capture)
(global-set-key (kbd "C-M-r") 'org-capture)

;; org-protocol
(require 'org-protocol)

(setq org-capture-templates
      '(
        ("w" "org-protocol bookmarks" item
         (file "bookmarks.org")
         "- [[%:link][%:description]]\n\n  %:initial"
         :empty-lines 1)
        ("q" "org-protocol quotes" item
         (file "quotes.org")
         "- %:initial"
         :empty-lines 1)
        )
      )

;; Encrypted org buffers
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(require 'org-element)

;; Org-drill
(require 'org-drill)

;; Org-agenda
(setq org-agenda-files
      (expand-file-name "agenda-files.org" org-directory))

(provide 'setup-org)
