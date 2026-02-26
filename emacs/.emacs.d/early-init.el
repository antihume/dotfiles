;; ᗜˬᗜ -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(defvar my--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist)))

(setq vc-handled-backends '(Git))

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      default-frame-alist '((horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen t
      inhibit-x-resources t
      initial-scratch-message nil)

;;; ᗜˬᗜ

(provide 'early-init)

;;; early-init.el ends here
