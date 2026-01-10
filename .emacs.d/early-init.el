;;; early-init.el --- Pre-initialization -*- lexical-binding: t -*-

;; Garbage collection threshold: high during startup, normal after
(setopt gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)   ; 16 MB
                  gc-cons-percentage 0.1)))

;; Prevent stale bytecode from shadowing newer source
(setopt load-prefer-newer t)

;; Disable UI elements before they're initialized (faster than init.el)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't resize frame based on font when loading
(setopt frame-inhibit-implied-resize t)

;; Prevent glimpse of unstyled Emacs
(setopt inhibit-redisplay t
        inhibit-message t)

(add-hook 'window-setup-hook
          (lambda ()
            (setq inhibit-redisplay nil
                  inhibit-message nil)
            (redisplay)))

;;; early-init.el ends here
