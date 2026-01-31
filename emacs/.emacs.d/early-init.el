;; -*- lexical-binding: t; -*-

(defvar my--file-name-handler-alist file-name-handler-alist)
(defvar my--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist
                  vc-handled-backends my--vc-handled-backends)))

(setq frame-inhibit-implied-resize 'force
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

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))
