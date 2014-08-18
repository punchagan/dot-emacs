(require 'helm)
(require 'helm-themes)

(defun helm-mini-or-imenu (imenu?)
  (interactive "P")
  (if imenu? (helm-imenu) (helm-mini)))

;; Why you look so ugly, helm!
(require 'color-theme)
(pc/after 'helm
  (load "color")

  (set-face-attribute 'helm-selection nil
                      :background (cdr (assoc 'cursor-color (color-theme-get-params)))
                      :foreground (cdr (assoc 'foreground-color (color-theme-get-params))))

  (set-face-attribute 'helm-source-header nil
                      :height 1.2
                      :foreground (cdr (assoc 'cursor-color (color-theme-get-params)))
                      :background nil)
  )

(global-set-key (kbd "C-c h") 'helm-mini-or-imenu)
(global-set-key (kbd "M-X") 'execute-extended-command)
(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'setup-helm)
