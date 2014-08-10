;;; Lisp mode configuration

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'pc/remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'pc/turn-on-paredit)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(provide 'setup-emacs-lisp)
