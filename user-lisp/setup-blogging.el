(require 'setup-python)

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
