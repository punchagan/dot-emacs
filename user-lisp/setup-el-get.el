(require 'el-get)

(el-get 'sync)

(add-hook 'el-get-post-install-hooks 'pc/el-get-post-install-hook)
(add-hook 'el-get-post-remove-hooks 'pc/el-get-post-remove-hook)

(provide 'setup-el-get)
