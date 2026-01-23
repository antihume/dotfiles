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
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setopt default-frame-alist `((horizontal-scroll-bars . nil)
			      (menu-bar-lines         . 0)
			      (tool-bar-lines         . 0)
			      (unsplittable           . nil)
			      (vertical-scroll-bars   . nil)))

(setopt gc-cons-percentage 0.5
	gc-cons-threshold most-positive-fixnum)

(defvar hume--file-name-handler-alist file-name-handler-alist)
(defvar hume--vc-handled-backends vc-handled-backends)

(setopt file-name-handler-alist nil
	vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 64 1024 1024)
                    gc-cons-percentage 0.1
                    file-name-handler-alist hume--file-name-handler-alist
		    vc-handled-backends hume--vc-handled-backends)))
