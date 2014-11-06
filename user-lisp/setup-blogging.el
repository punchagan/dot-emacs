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


(provide 'setup-blogging)
