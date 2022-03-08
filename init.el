;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Enable debug if required:1 starts here
;; (setq debug-on-error t)
;; Enable debug if required:1 ends here

;; Check that Emacs is not too old:1 starts here
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))
;; Check that Emacs is not too old:1 ends here

;; gc-thresholds starts here
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
;; gc-thresholds ends here

;; startup-code starts here
(defun pc/tangle-init-el ()
  "Tangle the file to update init.el file."
  (interactive "P")
  (let* ((time (current-time))
         (org-babel-tangle-comment-format-beg "%source-name starts here"))
    (when current-prefix-arg
      ;; Make and load init.el
      (org-babel-tangle)
      ;; Acknowledgement
      (message
       "Tangled  init.el … %.06f seconds."
       (float-time (time-since time))))))

(defun pc/load-init-el ()
  "Load the init.el file."
  (interactive)
  (let* ((time (current-time))
         (init-file (expand-file-name
                     "init.el"
                     (file-name-directory (buffer-file-name)))))

      ;; NOTE: Loading the init file doesn't work well...
      ;; We just tangle the file on save, and leave it at that. This
      ;; utility is left around, in case we occasionally chosee to
      ;; load the file manually. Ideally, when we are happy with the
      ;; config, we should just start a new Emacs instance.
      (setq pc/loading-tangled-init-p t)
      (load-file init-file)

      ;; Acknowledgement
      (message
       "Tangled, compiled, and loaded init.el … %.06f seconds. Restart Emacs if things get weird..."
       (float-time (time-since time)))))

;; Added this as a local variable in the org file
;; (add-hook 'after-save-hook 'org-babel-tangle nil 'local-to-this-file-please)
;; startup-code ends here

;; Basic user information:1 starts here
(setq user-full-name    "Puneeth Chaganti"
      user-mail-address "punchagan@muse-amuse.in")
;; Basic user information:1 ends here

;; System specific configuration:1 starts here
(pcase (system-name)
  ("haalbai" (setq pc/code-directory "~/code/"))
  ("chandrahara" (setq pc/code-directory "~/software/")))
;; System specific configuration:1 ends here

;; Setup ~package.el~:1 starts here
(require 'package)

(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

;; Update package list, unless previously updated. We do this only on the first
;; start-up. auth-update package takes care of it, for future startups
(unless (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory))
  (package-refresh-contents))

;; Ensure the installed packages are on load-path
(package-initialize)
;; Setup ~package.el~:1 ends here

;; Setup ~use-package~:1 starts here
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; ~use-package.el~ is no longer needed at runtime
;; Put the following at the top of your Emacs, to reduce load time:
(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)
;; Setup ~use-package~:1 ends here

;; Setup ~use-package~:2 starts here
(setq use-package-always-ensure t)
;; Setup ~use-package~:2 ends here

;; Auto update package:1 starts here
(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))
;; Auto update package:1 ends here

;; System packages used from with-in Emacs:1 starts here
;; Auto installing OS system packages
(use-package use-package-ensure-system-package
  :defer 4

  :config
  (setq system-packages-package-manager 'apt
        system-packages-use-sudo t
        system-packages-noconfirm t)
  (unless (boundp 'pc/loading-tangled-init-p)
    (system-packages-update)))

;; Please don't bother me when shell buffer names are in use, just make a new
;; buffer.
(setq async-shell-command-buffer 'new-buffer)

;; Display the output buffer for asynchronous shell commands only when the
;; command generates output.
(setq async-shell-command-display-buffer nil)

;; Don't ask me if I want to kill a buffer with a live process attached to it;
;; just kill it please.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
;; System packages used from with-in Emacs:1 ends here

;; Setup exec-path:1 starts here
(use-package exec-path-from-shell

  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))

  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))
;; Setup exec-path:1 ends here

;; Use authinfo gpg file:1 starts here
(setq auth-sources '("~/.authinfo.gpg"))
;; Use authinfo gpg file:1 ends here

;; Start Emacs server:1 starts here
(server-start)
;; Start Emacs server:1 ends here

;; Random Quote:1 starts here
(require 'json)

(defun pc/get-random-quote ()
  (let* ((json-array-type 'list)
         (quotes-file (expand-file-name "quotes.json" user-emacs-directory))
         (quotes (and (file-exists-p quotes-file)
                      (json-read-file quotes-file)))
         (n (random (length quotes)))
         (q (nth n quotes))
         (text (cdr (assoc 'body q)))
         (source (cdr (assoc 'source q))))
    (format "%s — %s" text source)))

(unless (boundp 'pc/quotes-timer)
  (setq pc/quotes-timer
        (run-with-idle-timer
         300
         'repeat-forever
         (lambda () (message (pc/get-random-quote))))))
;; Random Quote:1 ends here

;; Lean UI:1 starts here
;; No startup message
(setq inhibit-startup-message t)

(setq-default
 initial-scratch-message
 (format ";; Happy hacking, %s - Emacs ♥ you!\n\n" user-login-name))

;; No tool-bar, menu-bar and scroll-bar
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)

;; More prominent window divider
(window-divider-mode 1)
;; Lean UI:1 ends here

;; Basic Preferences:1 starts here
;; Basic preferences (taken from purcell)
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 confirm-kill-emacs (lambda (t) (y-or-n-p (format "%s\n%s" (pc/get-random-quote) t)))
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)
;; Basic Preferences:1 ends here

;; Basic Preferences:2 starts here
(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)
;; Basic Preferences:2 ends here

;; Basic Preferences:3 starts here
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable all ‘possibly confusing commands’ such as helpful but
;; initially-worrisome “narrow-to-region”, C-x n n.
(setq-default disabled-command-function nil)
;; Basic Preferences:3 ends here

;; Use UTF-8:1 starts here
(set-default-coding-systems 'utf-8-unix)
;; Use UTF-8:1 ends here

;; Fill column indicator:1 starts here
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?│)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))
;; Fill column indicator:1 ends here

;; ~diminish~ for modeline indicators:1 starts here
(use-package diminish
  :defer 3 ;; load after 5 seconds of idle time

  :config ;; Let's hide some markers.
  (diminish 'org-indent-mode))
;; ~diminish~ for modeline indicators:1 ends here

;; Use ~which-key~ for discovery:1 starts here
(use-package which-key
  :diminish
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.5))
;; Use ~which-key~ for discovery:1 ends here

;; Completion and Narrowing:1 starts here
(use-package counsel
  :diminish
  :ensure-system-package (ag . silversearcher-ag)
  :bind*                              ; load when pressed
  (("C-s"     . swiper)
   ("C-S-s" . counsel-ag)               ; Use ag to search the repo
   ("<f1> l"  . counsel-find-library)   ; find an Emacs Lisp library
   ("<f2> u"  . counsel-unicode-char))  ; insert a unicode symbol using a pop-up
  )
;; Completion and Narrowing:1 ends here

;; Completion and Narrowing:2 starts here
(use-package ivy
  :diminish
  :config
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; Number of lines to display
  (setq ivy-height 10)
  (setq ivy-count-format "[%d/%d] ")
  ;; no initial regexp by default (see original value using
  ;; `describe-variable')
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1)
  (counsel-mode 1))
;; Completion and Narrowing:2 ends here

;; Themes:1 starts here
;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)
;; Themes:1 ends here

;; Some code to pick and load a theme:1 starts here
(defun pc/load-theme (theme)
  "Apply user theme."
  (interactive
   (list
    (intern (ivy-read "Load custom theme: "
                      (mapcar #'symbol-name
                              (custom-available-themes))))))
  (progn
    ;; Disable all previously enabled themes
    (mapc 'disable-theme custom-enabled-themes)
    ;; Load chosen theme
    (load-theme theme)))
;; Some code to pick and load a theme:1 ends here

;; Ensure some nice themes are available:1 starts here
(use-package base16-theme)
(unless (boundp 'pc/loading-tangled-init-p)
  (pc/load-theme 'base16-humanoid-dark))
;; Ensure some nice themes are available:1 ends here

;; Symbol overlays:1 starts here
(use-package symbol-overlay
  :defer t
  :diminish t
  :hook
  (prog-mode . symbol-overlay-mode)
  (html-mode . symbol-overlay-mode)
  (yaml-mode . symbol-overlay-mode)
  (conf-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))
;; Symbol overlays:1 ends here

;; Rainbow delimiters:1 starts here
(use-package rainbow-delimiters
  :defer t
  :diminish t
  :hook
  (prog-mode . rainbow-delimiters-mode))
;; Rainbow delimiters:1 ends here

;; Subword and super-word modes:1 starts here
(with-eval-after-load 'subword
  (diminish 'subword-mode)
  (diminish 'superword-mode))
;; Subword and super-word modes:1 ends here

;; Large files:1 starts here
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

;; Use vlf package for very large files
(use-package vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))
;; Large files:1 ends here

;; Emacs backups:1 starts here
;; New location for backups.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Silently delete execess backup versions
(setq delete-old-versions t)

;; Only keep the last 3 backups of a file.
(setq kept-old-versions 3)

;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)

;; Use version numbers for backup files.
(setq version-control t)
;; Emacs backups:1 ends here

;; Whitespace:1 starts here
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Whitespace:1 ends here

;; Fill column:1 starts here
(setq-default fill-column 79)
;; Fill column:1 ends here

;; magit and git related stuff:1 starts here
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c b" . magit-blame)
  :custom
  ;; Show word diffs for current hunk
  (magit-diff-refine-hunk t)
  (magit-repository-directories `((,pc/code-directory . 3)
                                  ("~" . 0)
                                  ("~/.life-in-plain-text/" . 0)))
  ;; Do not ask about this variable when cloning.
  (magit-clone-set-remote.pushDefault t))
;; magit and git related stuff:1 ends here

;; Git helpers:1 starts here
;; Incremental blame?
(use-package git-blamed
  :defer t)

;; Major mode for editing git configuration files
(use-package git-modes
  :defer t)

;; Highlight diffs
(use-package diff-hl
  :defer
  :config
  (global-diff-hl-mode))
;; Git helpers:1 ends here

;; Magit helpers:1 starts here
(use-package magit-todos
  :config
  (setq magit-todos-exclude-globs '("*.css.map")))
;; Magit helpers:1 ends here

;; GitHub helpers:1 starts here
;; More generic is “browse-at-remote”.
;; Not very useful, if we have git-link?
;; (use-package github-browse-file :defer t)

;; Link to specific parts of a file
(use-package git-link :defer t)

;; Gists from Emacs
(use-package gist :defer t)

;; Turn references to PRs/Issues to clickable links
;; PR emacs/2 (Only the number is used -- emacs is ignored)
;; Bug 2 also works
(use-package bug-reference-github
  :hook
  (prog-mode . bug-reference-github-set-url-format))

(use-package github-review :defer t)

(use-package forge :after magit)
;; GitHub helpers:1 ends here

;; TODOs highlighting:1 starts here
;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  ;; I want todo-words highlighted in prose, not just in code fragements.
  :hook (org-mode . hl-todo-mode)
  :config
  ;; Enable it everywhere.
  (global-hl-todo-mode))
;; TODOs highlighting:1 ends here

;; TODOs highlighting:2 starts here
(use-package magit-todos
  :after magit
  :after hl-todo
  :config
  (magit-todos-mode))
;; TODOs highlighting:2 ends here

;; Setup projectile:1 starts here
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :custom
  (projectile-project-search-path `((,pc/code-directory . 3)))
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recently-active)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))
;; Setup projectile:1 ends here

;; Install ~ag~:1 starts here
(use-package ag
  :defer t)
;; Install ~ag~:1 ends here

;; yaml mode:1 starts here
(use-package yaml-mode)
;; yaml mode:1 ends here

;; Flycheck mode:1 starts here
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; Flycheck mode:1 ends here

;; Javascript:1 starts here
(setq js-indent-level 2)
;; Javascript:1 ends here

;; Prettier:1 starts here
(defun pc/enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(use-package prettier-js
  :defer t
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook #'(lambda ()
                               (pc/enable-minor-mode
                                '("\\.jsx?\\'" . prettier-js-mode)))))
;; Prettier:1 ends here

;; Use black in Python buffers:1 starts here
(use-package blacken
  :demand t
  :after python
  :hook (python-mode . blacken-mode))
;; Use black in Python buffers:1 ends here

;; Generate README from file header:1 starts here
(use-package md-readme)
;; Generate README from file header:1 ends here

;; Org mode:1 starts here
(use-package org
  :bind (("C-c c" . org-capture)
         (:map org-mode-map
               ("C-c C-q" . counsel-org-tag))))

(setq org-directory "~/.life-in-plain-text/src/")
(setq org-return-follows-link t)
;; Org mode:1 ends here

;; Paste HTML as org text:1 starts here
(defun pc/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "xclip -o -t text/html | pandoc -f html -t org"))
  (yank))
;; Paste HTML as org text:1 ends here

;; Org tags:1 starts here
(setq org-complete-tags-always-offer-all-agenda-tags t)
;; Org tags:1 ends here

;; Basic config:1 starts here
(setq org-agenda-files
      (expand-file-name "agenda-files.org" org-directory))

;; Enable a bunch of things, since we are going to use them, anyway..
(require 'org-clock)
(require 'org-agenda)
(require 'org-capture)

(setq org-enforce-todo-dependencies t)

;; Add a note whenever a task's deadline or scheduled date is changed.
(setq org-log-redeadline 'time)
(setq org-log-reschedule 'time)

;; How many days early a deadline item will begin showing up in your agenda list.
(setq org-deadline-warning-days 7)

;; In the agenda view, days that have no associated tasks will still have a line showing the date.
(setq org-agenda-show-all-dates t)

;; Scheduled items marked as complete will not show up in your agenda view.
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
;; Basic config:1 ends here

;; Super agenda:1 starts here
(use-package org-super-agenda
  :defer t)

(use-package org-ql
    :defer t)

;; FIXME: Add some filters and stuff to make it more useful?
;; Super agenda:1 ends here

;; Template to capture journal entries:1 starts here
(add-to-list 'org-capture-templates
             '("j"
               "Journal"
               entry
               (file+olp+datetree "journal.org")
               "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"))
;; Template to capture journal entries:1 ends here

;; Custom code to fire off journal mode:1 starts here
(defun pc/insert-journal-template ()
  (org-capture nil "j")
  (org-capture-finalize)
  (org-capture-goto-last-stored)
  (recenter-top-bottom 0)
  (org-cycle-hide-drawers 'all)
  (org-end-of-line))
;; Custom code to fire off journal mode:1 ends here

;; Custom code to fire off journal mode:2 starts here
(defun pc/journal (&optional mode)
  "Open a new frame for journaling.

If MODE is 'journal opens to the current day in the journal, and
creates a new day entry if not already present.

If MODE is 'clock jumps to the currently clocked entry, or prompt
one from the last few."
  (interactive)
  (pc/select-window-by-name "What are you doing?")
  ;; Display agenda...
  (org-agenda nil "a")
  (org-super-agenda-mode t)
  (org-agenda-log-mode t)
  (org-agenda-day-view)
  (org-agenda-goto-today)
  (delete-other-windows)
  (split-window-right)
  ;; Perform next action based on mode
  (cond
   ;; Show a capture buffer for a new journal entry
   ((equal mode 'journal)
    (org-capture nil "j"))
   ;; Show the current clock entry, if there's one. Otherwise prompt!
   ((equal mode 'clock)
    (org-clock-goto (not (org-clocking-p)))
    (org-narrow-to-subtree)
    (outline-show-subtree)
    (goto-char (buffer-end 1)))
   ;; Show today in the journal
   (t
    (org-capture-goto-target "j")
    (org-narrow-to-subtree))))

(defun pc/get-frame-by-name (title)
  "Return frame with the given TITLE.
If no such frame exists, creates a new frame."
  (or
   (car (filtered-frame-list
         (lambda (f)
           (string= title (cdr (assq 'title (frame-parameters f)))))))
   (make-frame
    `((title . ,title)
      (fullscreen . maximized)))))

(defun pc/select-window-by-name (title)
  "Raise the window with the specified TITLE."
  (let ((frame (pc/get-frame-by-name title)))
    (select-frame frame)
    (shell-command (format "wmctrl -R \"%s\"" title))))
;; Custom code to fire off journal mode:2 ends here

;; Work Today:1 starts here
(defun pc/work-today ()
  "Create a journal entry with today's work tasks"
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d"))
         (title "Notes for Today")
         (org-last-tags-completion-table
          (org-global-tags-completion-table
           (org-agenda-files)))
         (tags
          (org-completing-read "Tags:" #'org-tags-completion-function))
         (headlines (org-ql-query
                      :select '(org-get-heading t t t t)
                      :from (org-agenda-files)
                      :where `(and (clocked :on ,date) (tags tags)))))

    ;; Exit early if no matching headlines
    (when (not headlines)
      (user-error "No matching headlines"))

    (when (org-clocking-p)
      (org-clock-out))
    (pc/journal)
    (end-of-buffer)
    (org-insert-heading-after-current)
    (insert title)
    (org-set-tags tags)
    (end-of-buffer)
    (mapc (lambda (item) (insert (format "- %s\n" (org-no-properties item)))) headlines)))
;; Work Today:1 ends here

;; Org mode and Zulip:1 starts here
(use-package request :defer t)
(use-package ox-gfm :defer t)

(eval-and-compile
  (setq zulip-helpers-load-path
        (expand-file-name "my-repos/zulip-helpers.el" pc/code-directory)))

(use-package zulip-helpers
    :load-path zulip-helpers-load-path)
;; Org mode and Zulip:1 ends here

;; Org capture:1 starts here
(require 'org-protocol)
;; Org capture:1 ends here

;; Template to capture protocol entries:1 starts here
(add-to-list
   'org-capture-templates
   '("p"
     "Protocol"
     entry
     (file+olp+datetree "journal.org")
     "* %:description\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%:link\n\n#+begin_quote\n%i\n#+end_quote\n"))
;; Template to capture protocol entries:1 ends here

;; Org babel:1 starts here
(require 'org-tempo)
;; Org babel:1 ends here

;; Org reveal:1 starts here
(use-package ox-reveal :ensure t)
;; Org reveal:1 ends here

;; Org subtree to gist:1 starts here
(eval-and-compile
  (setq org2gist-load-path
        (expand-file-name "my-repos/org2gist" pc/code-directory)))

(use-package org2gist
  :load-path org2gist-load-path)

;; (require 'org2gist)
;; Org subtree to gist:1 ends here

;; Markdown:1 starts here
(use-package markdown-mode :defer t)
;; Markdown:1 ends here

;; Hugo & Blog setup:1 starts here
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)
;; Hugo & Blog setup:1 ends here

;; Hugo & Blog setup:2 starts here
(defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post."
  (let* ((date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
         (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* TODO " title " :noexport:")
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                 ":EXPORT_DESCRIPTION:"
                 ":EXPORT_HUGO_CUSTOM_FRONT_MATTER:"
                 ":END:"
                 "%?\n")          ;Place the cursor here finally
               "\n")))

(add-to-list 'org-capture-templates
             '("b"
               "Blog post for punchagan.muse-amuse.in"
               entry
               (file "blog-posts.org")
               (function org-hugo-new-subtree-post-capture-template)
               :prepend t))
;; Hugo & Blog setup:2 ends here

;; Emacs Anywhere:3 starts here
(defun pc/github-conversation-p (window-title)
  (or (string-match-p "Pull Request #" window-title)
      (string-match-p "Issue #" window-title)))

(defun pc/ea-popup-handler (app-name window-title x y w h)
  ;; set major mode
  (cond
   ;; ((pc/github-conversation-p window-title) (gfm-mode))
   ;; default major mode
   (t (org-mode))))
;; Emacs Anywhere:3 ends here

;; Emacs Anywhere:4 starts here
(use-package emacs-anywhere
  :defer 5
  :load-path "fake-lisp"

  :ensure-system-package
  ((xclip . xclip)
   (xdotool . xdotool)
   (xwininfo . xwininfo)
   ;; NOTE: The script itself checks for deps, and installing deps
   ;; asynchronously may cause the script to fail. Reloading the
   ;; requirement after other system deps are installed makes it work.
   ("~/.emacs_anywhere/bin/run" . "curl-bash-install https://raw.github.com/zachcurry/emacs-anywhere/master/install"))

  :config
  (add-hook 'ea-popup-hook 'pc/ea-popup-handler))
;; Emacs Anywhere:4 ends here
