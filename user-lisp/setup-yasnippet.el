(require 'yasnippet)

(yas/load-directory
 (expand-file-name
  "snippets"
  (file-name-directory (symbol-file 'yas-global-mode))))

(let ((user-snippets-dir
       (expand-file-name "snippets" user-emacs-directory)))
  (when (file-exists-p user-snippets-dir)
    (yas/load-directory user-snippets-dir)))

(yas-global-mode 1)


(provide 'setup-yasnippet)
