(require 'magit)

;; Global keybinding for magit status
(bind-key "C-M-g" 'magit-status)

;; All dirs to search for git repos
(setq magit-repo-dirs
      '("~/software/my-repos/" "~/software/random/" "~/.emacs.d"))

(provide 'setup-git)
