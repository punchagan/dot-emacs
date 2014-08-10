(require 'package)
(require 'setup-package-list)
(require 'find-func)

;; Add repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun pc/add-installed-packages-to-my-packages ()
  (let ((missing))
    (dolist (package package-alist)
      (let ((package-name (package-desc-name (cadr package))))
	(unless (member package-name my-packages)
	  (setq missing t)
	  (add-to-list 'my-packages package-name))))
    (when missing
      (pc/write-packages-to-config))))

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

(pc/add-installed-packages-to-my-packages)

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

(ad-activate 'package-install)
(ad-activate 'package-delete)

(defun packages-install (packages)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

;; Install any of my missing packages
(packages-install my-packages)

;; Seriously? Only UI delete for packages?
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


(provide 'setup-package)
