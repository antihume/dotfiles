(setopt frame-inhibit-implied-resize 'force
	frame-resize-pixelwise t
	inhibit-startup-buffer-menu t
	inhibit-startup-echo-area-message user-login-name
	inhibit-startup-screen t
	inhibit-x-resources t
	initial-scratch-message nil
	ring-bell-function 'ignore
	use-file-dialog nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setopt initial-frame-alist `((horizontal-scroll-bars . nil)
			      (menu-bar-lines . 0)
			      (tool-bar-lines . 0)
			      (unsplitabble . nil)
			      (vertical-scroll-bars . nil)))


(setopt gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.5)


(defvar prot-emacs--file-name-handler-alist file-name-handler-alist)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 100 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist prot-emacs--file-name-handler-alist
                  vc-handled-backends prot-emacs--vc-handled-backends)))
