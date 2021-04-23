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
(defun my/make-init-el ()
  "Tangle an init.el my init.org."
  (interactive "P") ;; Places value of universal argument into: current-prefix-arg
  (when current-prefix-arg
    (let* ((time      (current-time))
           (_date     (format-time-string "_%Y-%m-%d"))
           (init-file (expand-file-name  "init.el"
                                         (file-name-directory (buffer-file-name)))))

      ;; Make and load init.el
      (org-babel-tangle)
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

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Auto update packageset's set up [[https://github.com/rranelli/auto-package-update.el\][an auto-update mechanism\]].][Auto update packageset's set up [[https://github.com/rranelli/auto-package-update.el][an auto-update mechanism]].:1]]
(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))
;; Auto update packageset's set up [[https://github.com/rranelli/auto-package-update.el][an auto-update mechanism]].:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*System packages used from with-in Emacs][System packages used from with-in Emacs:1]]
;; Auto installing OS system packages
(use-package use-package-ensure-system-package
  :defer 4
  ;; Not sure, if I really want this, and may remove it in future. 
  :config
  (setq system-packages-package-manager 'apt
        system-packages-use-sudo t)
  (system-packages-update))

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
