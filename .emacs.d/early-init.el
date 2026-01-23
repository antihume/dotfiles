;; -*- lexical-binding: t; -*-

(setopt gc-cons-percentage 0.5
        gc-cons-threshold most-positive-fixnum)

(defvar my--file-name-handler-alist file-name-handler-alist)
(defvar my--vc-handled-backends vc-handled-backends)

(setopt file-name-handler-alist nil
        vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 64 1024 1024)
                    gc-cons-percentage 0.1
                    file-name-handler-alist my--file-name-handler-alist
                    vc-handled-backends my--vc-handled-backends)))

(setopt frame-inhibit-implied-resize 'force
        frame-resize-pixelwise t
        default-frame-alist '((horizontal-scroll-bars . nil)
                              (menu-bar-lines         . 0)
                              (tool-bar-lines         . 0)
                              (vertical-scroll-bars   . nil)))

(setopt inhibit-startup-buffer-menu t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-screen t
        inhibit-x-resources t
        initial-scratch-message nil
        ring-bell-function 'ignore
        use-file-dialog nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors 'silent))
