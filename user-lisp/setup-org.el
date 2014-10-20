(require 'org-element)

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
        ("t" "todo" entry (file+headline "todo.org" "Miscellaneous")
         "* TODO %?\n")
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

;; Org-passwords
(require 'org-passwords)
(setq org-passwords-file (expand-file-name "passwords.gpg" org-directory))
(setq org-passwords-random-words-dictionary "/etc/dictionaries-common/words")

;; Org-drill
(require 'org-drill)

;; Org todo
;;; defaults to TODO, DONE.  Can add more
;;; http://doc.norang.ca/org-mode.html#TodoKeywords

;; Org-agenda
(setq org-agenda-files
      (expand-file-name "agenda-files.org" org-directory))
(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-agenda-sticky t
      org-agenda-compact-blocks t
      org-agenda-include-diary t
      org-agenda-span 'day
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-agenda-start-with-log-mode t)

;;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote ((" " "ZTD Agenda"
               ((agenda "" nil)
                (tags "bigrock"
                      ((org-agenda-overriding-header "Big Rocks")
                       (org-tags-match-list-sublevels 'indented)
                       (org-agenda-sorting-strategy
                        '(category-keep))))
                )))))

;; org-habits
(require 'org-habit)

(provide 'setup-org)
