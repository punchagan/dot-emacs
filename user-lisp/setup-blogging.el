(require 'setup-python)

;; org2blog
(require 'metaweblog)
(require 'auth-source)
(require 'org2blog-autoloads)

(let ((credentials (auth-source-user-and-password "org2blog")))
  (setq org2blog/wp-blog-alist
        `(("lafootrix"
           :url "http://lafootrix.wordpress.com/xmlrpc.php"
           :username "punchagan"
           :default-title "Hello World"
           :default-categories ("org2blog" "emacs")
           :tags-as-categories nil)
          ("test"
           :url "http://testorg2blog.wordpress.com/xmlrpc.php"
           :username ,(car credentials)
           :password ,(cadr credentials)
           :default-title "Hello World"
           :default-categories ("org2blog" "emacs")
           :tags-as-categories nil
           :wp-code t))))

(setq org2blog/wp-use-sourcecode-shortcode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blogging related functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pc/nikola-site
  (expand-file-name "~/software/my-repos/muse-amuse.in/")
  "Path to the default nikola site.")

(defmacro pc/with-nikola-venv (&rest body)
  "Activate nikola venv, evaluate BODY, restore old venv."
  nil
  `(let ((old-venv venv-current-name) result)
     (venv-workon "nikola")
     (setq result (progn ,@body))
     (venv-workon old-venv)
     result))

(defun pc/nikola--tags-get ()
  "Get the current tags in the site, given the site path."
  (let* ((nikola-site (file-name-directory
                       (directory-file-name
                        (file-name-directory
                         (or (buffer-file-name (current-buffer)) "/")))))
         tags)
    (pc/with-nikola-venv
     (setq tags (shell-command-to-string
                (format "cd %s && nikola tags -l" nikola-site)))
     (unless (search "ERROR" tags)
       (cdr (split-string tags "\n" t "\s+"))))))

(defun pc/nikola-deploy ()
  (interactive)
  (pc/with-nikola-venv
   (async-shell-command (format "cd %s && nikola deploy" pc/nikola-site))))

(defun pc/nikola-new-post (title)
  (interactive "MTitle: ")
  (pc/with-nikola-venv
   (call-process-shell-command
    (format
     "export EDITOR=\"/home/punchagan/bin/emacsclient -n\" && cd %s && nikola new_post -e -t \"%s\"&"
     pc/nikola-site title))))

(defun pc/nikola-tags-insert ()
  "Insert a nikola tag at point."
  (interactive)
  (let* ((word-match (or (current-word t) ""))
         (tags (completing-read-multiple "Tag: " (pc/nikola--tags-get) nil nil word-match)))
    (when (and word-match tags)
      (delete-backward-char (length word-match)))
    (mapc (lambda (tag) (insert (format "%s, " tag))) tags)))

(provide 'setup-blogging)
