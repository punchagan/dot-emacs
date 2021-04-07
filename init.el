;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Enable%20debug%20if%20required][Enable debug if required:1]]
;; (setq debug-on-error t)
;; Enable debug if required:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*Check%20that%20Emacs%20is%20not%20too%20old][Check that Emacs is not too old:1]]
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))
;; Check that Emacs is not too old:1 ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::gc-thresholds][gc-thresholds]]
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
;; gc-thresholds ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::startup-code][startup-code]]
(defun my/make-init-el ()
  "Tangle an init.el my init.org."
  (interactive "P") ;; Places value of universal argument into: current-prefix-arg
  (when current-prefix-arg
    (let* ((time      (current-time))
           (_date     (format-time-string "_%Y-%m-%d"))
           (init-file (expand-file-name  "init.el"
                                         (file-name-directory (buffer-file-name)))))

      ;; Make and load init.el
      (org-babel-tangle)
      (load-file init-file)

      ;; Acknowledgement
      (message "Tangled, compiled, and loaded init.el … %.06f seconds"
               (float-time (time-since time))))))

;; Added this as a local variable in the org file, otherwise it doesn't work. :)
;; (add-hook 'after-save-hook 'my/make-init-el nil 'local-to-this-file-please)
;; startup-code ends here

;; [[file:~/software/my-repos/my-dot-emacs/init.org::*The%20=my/make-init-el=%20function][The =my/make-init-el= function:2]]
(message-box "yo")
;; The =my/make-init-el= function:2 ends here
