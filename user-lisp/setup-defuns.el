;; My defuns
(require 'find-func)

;; Advice package-install to update the package list in the config.
(defadvice package-install (after install-update-package-list (pkg))
  "Update the my-packages variable in the config."
  (add-to-list 'my-packages (if (package-desc-p pkg)
				(package-desc-name pkg) pkg))
  (pc/write-packages-to-config))

;; Advice package-delete to update the package list in the config.
(defadvice package-delete (after delete-update-package-list (pkg))
  "Update the my-packages variable in the config."
  (setq my-packages (remove (package-desc-name pkg) my-packages))
  (pc/write-packages-to-config))


(defun pc/add-installed-packages-to-my-packages ()
  "Add any installed packages not in my-packages list, to it."
  (let ((missing))

    ;; package-install
    (dolist (package package-alist)
      (let ((package-name (package-desc-name (cadr package))))
	(unless (member package-name my-packages)
	  (setq missing t)
	  (add-to-list 'my-packages package-name))))

    ;; el-get-install
    (let ((el-packages (el-get-list-package-names-with-status "installed")))
      (dolist (package el-packages)
        (unless (member package (mapcar 'symbol-name my-packages))
          (setq missing t)
          (add-to-list 'my-packages (make-symbol package)))))

    (when missing
      (pc/write-packages-to-config))))

(defun pc/el-get-post-install-hook (package)
  "Hook to run after a package has been removed"
  (add-to-list 'my-packages package)
  (pc/write-packages-to-config))

(defmacro pc/after (mode &rest body)
  "After MODE loads, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun pc/el-get-post-remove-hook (package)
  "Hook to run after a package has been removed"
  (setq my-packages (remove package my-packages))
  (pc/write-packages-to-config))

(defun pc/install (&optional package)
  (unless (ignore-errors (package-install package))
    (el-get-install package)))

(defun pc/nikola--tags-get ()
  "Get the current tags in the site, given the site path."
  (let* ((nikola-command
	  (expand-file-name "bin/nikola"
			    (venv-name-to-dir "nikola")))
         (nikola-site (file-name-directory
                       (directory-file-name
                        (file-name-directory
                         (or (buffer-file-name (current-buffer)) "/")))))
         (tags (shell-command-to-string
                (format "cd %s && %s tags -l" nikola-site nikola-command))))
    (unless (search "ERROR" tags)
      (cdr (split-string tags "\n" t "\s+")))))

(defun pc/nikola-tags-insert ()
  "Insert a nikola tag at point."
  (interactive)
  (let* ((word-match (or (current-word t) ""))
         (tags (completing-read-multiple "Tag: " (pc/nikola--tags-get) nil nil word-match)))
    (when (and word-match tags)
      (delete-backward-char (length word-match)))
    (mapc (lambda (tag) (insert (format "%s, " tag))) tags)))

(defun pc/package-delete (package)
  "Provide a way to delete packages from the CLI."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (package-initialize t)
     (list (intern (completing-read
                    "Delete package: "
                    (mapcar (lambda (elt) (symbol-name (car elt)))
                            package-alist)
                    nil t)))))
  (package-delete (cadr (assq package package-alist)))
  (package-initialize t))

(defun pc/packages-install (packages)
  "Install any packages that are missing."
  (dolist (package my-packages)
    (unless (or (package-installed-p package)
                (el-get-package-installed-p package))
      (pc/install package))))

(defun pc/read-random-article ()
  "Jump to and open a random article.  The way the function is
  currently written requires read articles to be archived."
  (interactive)
  (save-excursion
    (save-restriction

      ;; find all articles, headline
      (org-goto-marker-or-bmk
       (org-find-exact-heading-in-directory "Articles To Read" org-directory))
      (org-narrow-to-subtree)
      (org-show-subtree)
      (org-clock-in)

      ;; jump to a random subtree
      (org-goto-first-child)
      (forward-line (random (count-lines (point) (buffer-end 1))))
      (org-back-to-heading)

      ;; open the link
      (org-next-link)
      (org-return))))

(defun pc/remove-elc-on-save ()
  "Remove the .elc files when saving a .el file."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (let ((elc (concat buffer-file-name "c")))
                (if (file-exists-p elc)
                    (delete-file elc))))))

(defun pc/save-visible-windows ()
  "Function to save all the buffers in visible windows in the
  current frame."
  (dolist (window (window-list))
    (let ((buffer (window-buffer window)))
      (when (and (buffer-modified-p buffer)
                 (buffer-file-name buffer))
        (save-buffer)))))

(defun pc/sort--end-record ()
  (forward-sexp))

(defun pc/sort--goto-first-defun-in-buffer ()
  (goto-char (buffer-end -1))
  (search-forward "(defun " nil t 1)
  (beginning-of-line))

(defun pc/sort--next-record ()
  (if (search-forward "(defun " nil t 1)
      (beginning-of-line)
    (goto-char (buffer-end 1))))

(defun pc/sort-defuns-in-buffer ()
  "Sort all the functions defined in the buffer"
  (interactive)
  (pc/sort--goto-first-defun-in-buffer)
  (sort-subr nil 'pc/sort--next-record 'pc/sort--end-record))

(defun pc/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun pc/turn-off-notifications ()
  "Turn gnome notifications off."
  ;; gnome notifications
  (dbus-send-signal
   :session
   "org.gnome.SessionManager"
   "/org/gnome/SessionManager/Presence"
   "org.gnome.SessionManager.Presence"
   "SetStatus" 2)
  ;; pidgin status
  (shell-command "purple-remote setstatus?status=unavailable")
  ;; turn off tracking mode
  (erc-track-disable))

(defun pc/turn-on-line-and-column-numbering ()
  (make-local-variable 'column-number-mode)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun pc/turn-on-notifications ()
  "Turn gnome notifications back on."
  ;; gnome notifications
  (dbus-send-signal
   :session
   "org.gnome.SessionManager"
   "/org/gnome/SessionManager/Presence"
   "org.gnome.SessionManager.Presence"
   "SetStatus" 0)
  ;; pidgin status
  (shell-command "purple-remote setstatus?status=available")
  ;; turn on tracking mode
  (erc-track-enable))

(defun pc/turn-on-paredit ()
  (require 'paredit)
  (paredit-mode +1))

(defun pc/write-packages-to-config ()
  "Update my-packages in the config, based on the current-value."
  (sort (delete-dups my-packages) 'string<)
  (find-function-do-it 'my-packages 'defvar 'switch-to-buffer)
  (kill-sexp)
  (insert (format "(defvar my-packages \n  '%s)" my-packages))
  (backward-sexp)
  (fill-paragraph)
  (save-buffer)
  (kill-buffer))

(provide 'setup-defuns)
