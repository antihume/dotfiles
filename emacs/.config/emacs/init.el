;; ᗜˬᗜ -*- lexical-binding: t; -*-

(use-package emacs
  :preface
  (defun narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing
           ;; command. Remove this first conditional if
           ;; you don't want it.
           (cond ((ignore-errors (org-edit-src-code) t)
                  (delete-other-windows))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))
  :custom
  (auto-window-vscroll nil)
  (cursor-type 'bar)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (disabled-command-function nil)
  (display-line-numbers-type 'relative)
  (fast-but-imprecise-scrolling t)
  (fill-column 80)
  (indent-tabs-mode nil)
  (inhibit-splash-screen t)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (native-comp-async-report-warnings-errors 'silent)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (redisplay-skip-fontification-on-input t)
  (ring-bell-function 'ignore)
  (save-interprogram-paste-before-kill t)
  (scroll-conservatively 1000)
  (scroll-preserve-screen-position t)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (text-mode-ispell-word-completion nil)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (window-combination-resize t)
  (x-stretch-cursor t)
  :init
  (column-number-mode)
  (context-menu-mode)
  (delete-selection-mode)
  (editorconfig-mode)
  (global-goto-address-mode)
  (global-so-long-mode)
  (menu-bar-mode -1)
  (minibuffer-depth-indicate-mode)
  (pixel-scroll-precision-mode)
  (repeat-mode)
  (save-place-mode)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  :config
  (set-face-attribute 'default nil :family "Iosevka SS08" :height 120)
  :hook
  ((prog-mode . display-line-numbers-mode)
   (prog-mode . electric-pair-local-mode))
  :bind
  (("C-c d" . duplicate-dwim)
   ("C-c m" . narrow-or-widen-dwim)
   ("C-x k" . kill-current-buffer)
   ("M-/" . hippie-expand)
   ("M-c" . capitalize-dwim)
   ("M-l" . downcase-dwim)
   ("M-u" . upcase-dwim)
   ("M-z" . zap-up-to-char)))

(use-package compile
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-free-space nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-AGFhlv --group-directories-first")
  (dired-no-confirm t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions t))

(use-package dired-x
  :after dired
  :custom
  (dired-omit-files "^\\.[^.].*\\|~$")
  (dired-omit-verbose nil)
  :hook
  (dired-mode . dired-omit-mode))

(use-package project
  :custom
  (project-vc-extra-root-markers
   '(".project" "Makefile" "setup.py" "package.json"
     "Cargo.toml" "go.mod")))

(use-package org
  :custom
  (org-agenda-files '("~/org"))
  (org-directory "~/org")
  (org-log-done 'time)
  (org-preview-latex-default-process 'dvisvgm)
  (org-return-follows-link t)
  (org-startup-folded 'content)
  (org-startup-with-latex-preview t)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (shell . t)))
  (plist-put org-format-latex-options :scale 2.0)
  :hook
  (org-mode . variable-pitch-mode))

(use-package whitespace
  :custom
  (whitespace-style '(face trailing))
  :init
  (global-whitespace-mode))

(use-package ibuffer
  :custom
  (ibuffer-expert t)
  :bind
  ("C-x C-b" . ibuffer-other-window))

(use-package hl-line
  :init
  (global-hl-line-mode)
  :hook
  ((prog-mode text-mode) .(lambda ()
                            (setq-local global-hl-line-mode nil))))

(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (isearch-lazy-highlight 'all-windows)
  (lazy-highlight-initial-delay 0))

(use-package recentf
  :init
  (recentf-mode)
  :bind
  ("C-x C-r" . recentf-open))

(use-package savehist
  :custom
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring compile-history corfu-history))
  :init
  (savehist-mode))

(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-show 1)
  :config
  (tab-bar-history-mode)
  (tab-bar-mode))

(use-package treesit
  :custom
  (treesit-enabled-modes t)
  (treesit-font-lock-level 4))

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-flag t)
  (uniquify-buffer-name-style 'forward))

(use-package window
  :preface
  (let ((windows nil) (index 0))
    (defun other-window-mru ()
      "Cycle windows in most-recently-used order."
      (interactive)
      (if (one-window-p t)
          (message "No other window to select")
        (if (eq last-command this-command)
            (setq index (mod (1+ index) (length windows)))
          (select-window (selected-window))
          (setq windows (sort (window-list nil 'nomini)
                              :key #'window-use-time :reverse t)
                index 1))
        (select-window (nth index windows) 'norecord))))
  :bind
  (("C-x o" . other-window-mru)
   :repeat-map other-window-repeat-map
   ("o" . other-window-mru)))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config 0))

(use-package files
  :init
  (make-directory (expand-file-name "backup/" user-emacs-directory) t)
  (make-directory (expand-file-name "auto-save/" user-emacs-directory) t)
  :custom
  (confirm-kill-emacs #'yes-or-no-p)
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
  (backup-by-copying t)
  (backup-directory-alist
   `(("." . ,(expand-file-name "backup/" user-emacs-directory))))
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (find-file-visit-truename t)
  (require-final-newline t)
  (version-control t))

(use-package paren
  :custom
  (show-paren-context-when-offscreen 'overlay))

(use-package vc
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

(use-package autorevert
  :init
  (global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t)
  (global-auto-revert-non-file-buffers t))

(use-package which-key
  :init
  (which-key-mode))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode t)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-italic-constructs t)
  :config
  (modus-themes-load-theme 'ef-dream))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :custom
  (vertico-multiform-categories '((consult-grep buffer)
                                  (consult-location buffer)
                                  (xref-location buffer)
                                  (history reverse)
                                  (kill-ring reverse)
                                  (file flat (:keymap . vertico-directory-map))
                                  (t flat)))
  (vertico-multiform-commands '((consult-buffer reverse)
                                (consult-compile-error reverse)
                                (consult-flymake reverse)
                                (consult-global-mark reverse)
                                (consult-imenu reverse)
                                (consult-imenu-multi reverse)
                                (consult-mark reverse)
                                (consult-outline reverse)))
  :init
  (vertico-multiform-mode))

(use-package vertico-quick
  :after vertico
  :bind
  (:map vertico-map
        ("M-j" . vertico-quick-exit)))

(use-package vertico-repeat
  :after vertico
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :bind
  ("C-c r" . vertico-repeat))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   ("M-y" . consult-yank-pop)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-find)
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :custom
  (consult-narrow-key "C-+")
  (register-preview-delay 0.5)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-find consult-locate consult-bookmark consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
        ([remap next-line] . nil)
        ([remap previous-line] . nil))
  :config
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send))))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :bind-keymap
  ("M-+" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :preface
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets.  Uses `which-key-idle-delay' before showing the popup,
and `which-key-idle-secondary-delay' for subsequent updates."
    (let ((timer nil) (shown nil))
      (lambda (&optional keymap targets prefix)
        (when timer (cancel-timer timer) (setq timer nil))
        (if (null keymap)
            (progn
              (setq shown nil)
              (which-key--hide-popup-ignore-command))
          (let ((name (if (eq (plist-get (car targets) :type) 'embark-become)
                          "Become"
                        (format "Act on %s '%s'%s"
                                (plist-get (car targets) :type)
                                (embark--truncate-target (plist-get (car targets) :target))
                                (if (cdr targets) "…" ""))))
                (km (if prefix
                        (pcase (lookup-key keymap prefix 'accept-default)
                          ((and (pred keymapp) km) km)
                          (_ (key-binding prefix 'accept-default)))
                      keymap)))
            (setq timer
                  (run-with-idle-timer
                   (if shown
                       (or which-key-idle-secondary-delay which-key-idle-delay)
                     which-key-idle-delay)
                   nil
                   (lambda ()
                     (setq shown t timer nil)
                     (which-key--show-keymap
                      name km nil nil t
                      (lambda (binding)
                        (not (string-suffix-p "-argument" (cdr binding)))))))))))))
  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators
   '(embark-which-key-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :config
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t)

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-define-global-key-bindings 'recommended))

(use-package expreg
  :ensure t
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              (add-to-list 'expreg-functions #'expreg--sentence)))
  :bind
  (("C--" . expreg-contract)
   ("C-=" . expreg-expand)))

(use-package avy
  :ensure t
  :preface
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  :bind
  (("M-j" . avy-goto-word-1)
   :map isearch-mode-map
   ("M-j" . avy-isearch))
  :custom
  (avy-background t)
  (avy-timeout-seconds 0.35)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

;;; init.el ends here
