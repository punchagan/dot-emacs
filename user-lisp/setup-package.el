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

(pc/add-installed-packages-to-my-packages)

(ad-activate 'package-install)
(ad-activate 'package-delete)

;; Install any of my missing packages
(pc/packages-install my-packages)

(provide 'setup-package)
