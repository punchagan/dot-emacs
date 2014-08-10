;; Put at the beginning to avoid momentary display
(when window-system
  (scroll-bar-mode -1)
  (mouse-wheel-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; No menubar
(menu-bar-mode -1)

;; No blinking cursor
(blink-cursor-mode -1)

;; No startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 1))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format '("" invocation-name " - "
                           (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b"))))

;; use wombat theme
(load-theme 'wombat t)
;; the cursor is too dark, though...
(set-cursor-color "#cccccc")

(provide 'setup-appearance)
