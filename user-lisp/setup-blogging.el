(require 'setup-python)

;; Nikola
;; Get tags used in the nikola site, using the tags plugin.
(defun pc/nikola-tags-get ()
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
         (tags (completing-read-multiple "Tag: " (pc/nikola-tags-get) nil nil word-match)))
    (when (and word-match tags)
      (delete-backward-char (length word-match)))
    (mapc (lambda (tag) (insert (format "%s, " tag))) tags)))


;; org2blog
(require 'metaweblog)
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("lafootrix"
	 :url "http://lafootrix.wordpress.com/xmlrpc.php"
	 :username "punchagan"
	 :default-title "Hello World"
	 :default-categories ("org2blog" "emacs")
	 :tags-as-categories nil)
	("org2blog"
	 :url "http://testorg2blog.wordpress.com/xmlrpc.php"
	 :username "org2blog"
	 :default-title "Hello World"
	 :default-categories ("org2blog" "emacs")
	 :tags-as-categories nil
	 :wp-code t)))

(setq org2blog/wp-use-sourcecode-shortcode t)


(provide 'setup-blogging)
