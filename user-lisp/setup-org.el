(require 'org-element)

;; Org-directory
(setq org-directory "~/.life-in-plain-text/")

;; Babel setup
(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (ditaa . t)
			       (python . t)
			       (sh . t)))

(setq org-babel-sh-command "bash")

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
        ;; General
        ("t" "todo" entry (file+headline "todo.org" "Miscellaneous")
         "* TODO %?\n\n"
         :empty-lines 1)
        ("c" "org-protocol links under clock item" item
         (clock)
         "- [[%:link][%:description]]\n\n  %:initial"
         :immediate-finish t :empty-lines 1)

        ;; Blog related
        ("b" "org-protocol bookmarks" item
         (file "bookmarks.org")
         "- [[%:link][%:description]]\n\n  %:initial"
         :empty-lines 1)
        ("q" "org-protocol quotes" item
         (file "quotes.org")
         "- %:initial"
         :empty-lines 1)

        ;; Incremental reading
        ("u"
         "Task: Read this URL"
         entry
         (file+headline "todo.org" "Articles To Read")
         "* TODO Read article: [[%:link][%:description]]\n\n  %:initial\n\n"
         :empty-lines 1
         :immediate-finish t)
        ("w"
         "Capture web snippet"
         entry
         (file+headline "notes.org" "Web notes")
         "%(concat  \"* Fact: '%:description'        :\"
         (format \"%s\" org-drill-question-tag)
         \":\n:PROPERTIES:\n:DATE_ADDED: %u\n:SOURCE_URL: %:link\n:END:\n\n%x\n%?\n\n\")"
         :empty-lines 1
         )
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
(setq org-drill-scope
      `("/media/punchagan/f9213ab4-a7aa-40c0-91c0-a3d60af751f3/videos/technical/CS_1156x_Learning_from_data/notes.org"
        "/media/punchagan/f9213ab4-a7aa-40c0-91c0-a3d60af751f3/videos/technical/mmds-001/notes.org"
        ,(expand-file-name "notes.org" org-directory)))

;; Org todo
;;; Taken from http://doc.norang.ca/org-mode.html#TodoKeywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))

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
      org-agenda-start-with-log-mode t
      org-agenda-skip-scheduled-if-done t
      org-clock-persist t)

;;; Clocking
(bind-keys
 :prefix "<f9>"
 :prefix-map pc/clock
 ;; except org-clock-in, everything is useful globally...
 ("i" . org-clock-in)
 ("l" . org-clock-in-last)
 ("o" . org-clock-out)
 ("x" . org-clock-cancel)
 ("j" . org-clock-goto)
 ("e" . org-set-effort))
;;; The keybindings are similar to what org-agenda already has. I, O, X, J, e.

;;; What to do with dangling clocks?
(org-clock-persistence-insinuate)

;; Clocking and notifications
(add-hook 'org-clock-in-hook 'pc/turn-off-notifications)
(add-hook 'org-clock-out-hook 'pc/turn-on-notifications)

;;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote ((" " "ZTD Agenda"
               ((agenda "" nil)
                (tags "bigrock"
                      ((org-agenda-overriding-header "Big Rocks")
                       (org-tags-match-list-sublevels nil)
                       (org-agenda-sorting-strategy
                        '(category-keep))))
                )))))

;; org-habits
(require 'org-habit)

;; Export
(setq org-use-sub-superscripts '{})

;; org-file apps
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . "~/bin/xpdf %s")
        ("\\.pdf::\\([0-9]+\\)\\'" . "~/bin/xpdf \"%s\" %1")))

;; clip2org
(require 'clip2org)
(setq clip2org-include-date t)
(setq clip2org-clippings-file "/media/punchagan/Kindle/documents/My Clippings.txt")


(provide 'setup-org)
