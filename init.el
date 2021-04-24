;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Enable debug if required][Enable debug if required:1]]
;; (setq debug-on-error t)
;; Enable debug if required:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Check that Emacs is not too old][Check that Emacs is not too old:1]]
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))
;; Check that Emacs is not too old:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::gc-thresholds][gc-thresholds]]
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
;; gc-thresholds ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::startup-code][startup-code]]
(defun pc/make-init-el (&optional arg)
  "Tangle an init.el my init.org."
  (interactive "P")
  (when arg
    (let* ((time      (current-time))
           (_date     (format-time-string "_%Y-%m-%d"))
           (init-file (expand-file-name  "init.el"
                                         (file-name-directory (buffer-file-name)))))

      ;; Make and load init.el
      (org-babel-tangle)
      (setq pc/loading-tangled-init-p t)
      (load-file init-file)

      ;; Acknowledgement
      (message "Tangled, compiled, and loaded init.el … %.06f seconds"
               (float-time (time-since time))))))

;; Added this as a local variable in the org file, otherwise it doesn't work. :)
;; (add-hook 'after-save-hook 'my/make-init-el nil 'local-to-this-file-please)
;; startup-code ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Basic user information][Basic user information:1]]
(setq user-full-name    "Puneeth Chaganti"
      user-mail-address "punchagan@muse-amuse.in")
;; Basic user information:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Setup ~package.el~][Setup ~package.el~:1]]
(require 'package)

(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

;; Update local list of available packages. We don't want to do this
;; on start-up, right?
;; (package-refresh-contents)

;; Ensure the installed packages are on load-path
(package-initialize)
;; Setup ~package.el~:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Setup ~use-package~][Setup ~use-package~:1]]
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; ~use-package.el~ is no longer needed at runtime
;; Put the following at the top of your Emacs, to reduce load time:
(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)
;; Setup ~use-package~:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Setup ~use-package~][Setup ~use-package~:2]]
(setq use-package-always-ensure t)
;; Setup ~use-package~:2 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Auto update package][Auto update package:1]]
(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))
;; Auto update package:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*System packages used from with-in Emacs][System packages used from with-in Emacs:1]]
;; Auto installing OS system packages
(use-package use-package-ensure-system-package
  :defer 4

  :config
  (setq system-packages-package-manager 'apt
        system-packages-use-sudo t)
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Setup exec-path][Setup exec-path:1]]
(use-package exec-path-from-shell

  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))

  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))
;; Setup exec-path:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Use authinfo gpg file][Use authinfo gpg file:1]]
(setq auth-sources '("~/.authinfo.gpg"))
;; Use authinfo gpg file:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Start Emacs server][Start Emacs server:1]]
(server-start)
;; Start Emacs server:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Lean UI][Lean UI:1]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Basic Preferences][Basic Preferences:1]]
;; Basic preferences (taken from purcell)
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 confirm-kill-emacs 'y-or-n-p
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Basic Preferences][Basic Preferences:2]]
(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)
;; Basic Preferences:2 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Basic Preferences][Basic Preferences:3]]
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable all ‘possibly confusing commands’ such as helpful but
;; initially-worrisome “narrow-to-region”, C-x n n.
(setq-default disabled-command-function nil)
;; Basic Preferences:3 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Fill column indicator][Fill column indicator:1]]
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?│)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))
;; Fill column indicator:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*~diminish~ for modeline indicators][~diminish~ for modeline indicators:1]]
(use-package diminish
  :defer 3 ;; load after 5 seconds of idle time

  :config ;; Let's hide some markers.
  (diminish 'org-indent-mode))
;; ~diminish~ for modeline indicators:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Use ~which-key~ for discovery][Use ~which-key~ for discovery:1]]
(use-package which-key
  :diminish
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.5))
;; Use ~which-key~ for discovery:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Completion and Narrowing][Completion and Narrowing:1]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Completion and Narrowing][Completion and Narrowing:2]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Themes][Themes:1]]
;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)
;; Themes:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Some code to pick and load a theme][Some code to pick and load a theme:1]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Ensure some nice themes are available][Ensure some nice themes are available:1]]
(use-package base16-theme)
(unless (boundp 'pc/loading-tangled-init-p)
  (pc/load-theme 'base16-humanoid-dark))
;; Ensure some nice themes are available:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Symbol overlays][Symbol overlays:1]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Rainbow delimiters][Rainbow delimiters:1]]
(use-package rainbow-delimiters
  :defer t
  :diminish t
  :hook
  (prog-mode . rainbow-delimiters-mode))
;; Rainbow delimiters:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Subword and super-word modes][Subword and super-word modes:1]]
(with-eval-after-load 'subword
  (diminish 'subword-mode)
  (diminish 'superword-mode))
;; Subword and super-word modes:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Large files][Large files:1]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Emacs backups][Emacs backups:1]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*magit and git related stuff][magit and git related stuff:1]]
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c b" . magit-blame)
  :custom
  ;; Show word diffs for current hunk
  (magit-diff-refine-hunk t)
  ;; Do not ask about this variable when cloning.
  (magit-clone-set-remote.pushDefault t))
;; magit and git related stuff:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Git helpers][Git helpers:1]]
;; Incremental blame?
(use-package git-blamed
  :defer t)

;; Major mode to edit git ignore files
(use-package gitignore-mode
  :defer t)

;; Major mode to edit git config files
(use-package gitconfig-mode
  :defer t)

;; Highlight diffs
(use-package diff-hl
  :defer
  :config
  (global-diff-hl-mode))
;; Git helpers:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Magit helpers][Magit helpers:1]]
(use-package magit-todos)
;; Magit helpers:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*GitHub helpers][GitHub helpers:1]]
;; More generic is “browse-at-remote”.
;; Not very useful, if we have git-link?
;; (use-package github-browse-file :defer t)

;; Link to specific parts of a file
(use-package git-link :defer t)

;; Gists from Emacs
(use-package yagist :defer t)

;; Turn references to PRs/Issues to clickable links
;; PR emacs/2 (Only the number is used -- emacs is ignored)
;; Bug 2 also works
(use-package bug-reference-github
  :hook
  (prog-mode . bug-reference-github-set-url-format))

(use-package github-review :defer t)

(use-package forge :after magit)
;; GitHub helpers:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*TODOs highlighting][TODOs highlighting:1]]
;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  ;; I want todo-words highlighted in prose, not just in code fragements.
  :hook (org-mode . hl-todo-mode)
  :config
  ;; Enable it everywhere.
  (global-hl-todo-mode))
;; TODOs highlighting:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*TODOs highlighting][TODOs highlighting:2]]
(use-package magit-todos
  :after magit
  :after hl-todo
  :config
  (magit-todos-mode))
;; TODOs highlighting:2 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Basic config][Basic config:1]]
(setq org-directory "~/.life-in-plain-text/src/")
(setq org-agenda-files
      (expand-file-name "agenda-files.org" org-directory))

;; Enable a bunch of things, since we are going to use them, anyway..
(require 'org-clock)
(require 'org-agenda)
(require 'org-capture)
;; Basic config:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Super agenda][Super agenda:1]]
(use-package org-super-agenda
  :defer t)

(use-package org-ql
    :defer t)

;; FIXME: Add some filters and stuff to make it more useful?
;; Super agenda:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Template to capture entries][Template to capture entries:1]]
(add-to-list 'org-capture-templates
             '("j"
               "Journal"
               entry
               (file+olp+datetree "journal.org")
               "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"))
;; Template to capture entries:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Custom code to fire off journal mode][Custom code to fire off journal mode:1]]
(defun pc/insert-journal-template ()
  (org-capture nil "j")
  (org-capture-finalize)
  (org-capture-goto-last-stored)
  (org-cycle-hide-drawers 'all)
  (org-end-of-line))
;; Custom code to fire off journal mode:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Custom code to fire off journal mode][Custom code to fire off journal mode:2]]
(defun pc/journal ()
  "Open a new frame for journaling.
    - Jumps to the currently clocked item, if there is one.

    - Otherwise, opens to the current day in the journal, and creates
      a new day entry if not already present."
  (interactive)
  (let* ((title "What are you doing?")
         (frame (or
                 (car (filtered-frame-list
                       (lambda (f)
                         (string= title (cdr (assq 'title (frame-parameters f)))))))
                 (make-frame
                  `((title . ,title)
                    (user-position . t)
                    (left . (+ 550))
                    (top . (+ 400))
                    (width . 120)
                    (height . 40))))))
    (select-frame frame)
    (org-agenda nil "a")
    (org-super-agenda-mode t)
    (org-agenda-log-mode t)
    (org-agenda-day-view)
    (org-agenda-goto-today)
    (delete-other-windows)
    (split-window-right)
    (if (org-clocking-p)
        (org-clock-goto)
      (pc/insert-journal-template))
    (org-narrow-to-subtree)
    (outline-show-subtree)
    (when (org-clocking-p)
      (goto-char (buffer-end 1)))
    (shell-command (format "wmctrl -a \"%s\"" title))))
;; Custom code to fire off journal mode:2 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Emacs Anywhere][Emacs Anywhere:3]]
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Emacs Anywhere][Emacs Anywhere:4]]
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
