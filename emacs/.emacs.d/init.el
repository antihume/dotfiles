;; -*- lexical-binding: t; -*-

;;; Package management

(use-package package
  :custom
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                      ("melpa" . "https://melpa.org/packages/")))
  (package-archive-priorities '(("gnu" . 3)
                                ("nongnu" . 2)
                                ("melpa" . 1))))

;;; Core configuration

(use-package emacs
  :preface
  (defvar my/font-family "Iosevka SS12"
    "Default font family.")

  (defvar my/font-height 120
    "Default font height in 1/10pt units.")

  (defun my--enable-show-trailing-whitespace ()
    (setq-local show-trailing-whitespace t))

  (defun my--enable-delete-trailing-whitespace ()
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  (defun my/find-file-with-sudo ()
    "Find file as root via TRAMP sudo."
    (interactive)
    (let ((default-directory
           (concat "/sudo::" (or (file-name-directory (or (buffer-file-name) "")) "/"))))
      (call-interactively #'find-file)))

  (defun my/path-kill-ring-save ()
    "Save current file path to kill ring."
    (interactive)
    (if-let* ((filename (buffer-file-name)))
	(progn
          (kill-new filename)
          (message "Copied: %s" filename))
      (user-error "Buffer is not visiting a file")))

  (defun my/scratch-buffer-current-mode ()
    "Switch to a scratch buffer in the current major mode."
    (interactive)
    (let* ((mode major-mode)
           (name (format "*scratch-%s*" mode))
           (buf (get-buffer-create name)))
      (switch-to-buffer buf)
      (unless (eq major-mode mode)
	(funcall mode))))

  :custom

  ;; General behavior
  (create-lockfiles nil)
  (custom-file null-device)
  (disabled-command-function nil)
  (ring-bell-function 'ignore)
  (use-file-dialog nil)
  (use-short-answers t)

  ;; Kill ring
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)

  ;; Scrolling
  (pixel-scroll-precision-use-momentum t)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (scroll-step 1)

  ;; Search
  (isearch-lazy-count t)
  (lazy-highlight-initial-delay 0)

  ;; Minibuffer
  (enable-recursive-minibuffers t)
  (minibuffer-visible-completions t)

  ;; Parens
  (blink-matching-paren t)
  (show-paren-context-when-offscreen 'overlay)
  (show-paren-delay 0)

  ;; Line numbers
  (display-line-numbers-type 'relative)

  ;; Help
  (apropos-do-all t)
  (describe-bindings-outline t)
  (help-window-select t)

  ;; Navigation
  (recenter-positions '(top middle bottom))
  (set-mark-command-repeat-pop t)

  ;; Compilation
  (next-error-message-highlight t)

  ;; Which-key
  (which-key-idle-delay 0.5)

  ;; Windmove
  (windmove-default-keybindings 'super)

  ;; Mode line
  (mode-line-compact 'long)

  ;; Uniquify
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/")

  ;; Recentf
  (recentf-max-saved-items 200)

  ;; Midnight
  (clean-buffer-list-delay-general 1)

  :config

  ;; Font
  (when (member my/font-family (font-family-list))
    (set-face-attribute 'default nil
                        :family my/font-family
                        :height my/font-height))

  ;; Global modes
  (column-number-mode 1)
  (context-menu-mode 1)
  (delete-selection-mode 1)
  (editorconfig-mode 1)
  (global-auto-revert-mode 1)
  (global-goto-address-mode 1)
  (global-so-long-mode 1)
  (midnight-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (pixel-scroll-precision-mode 1)
  (recentf-mode 1)
  (repeat-mode 1)
  (save-place-mode 1)
  (savehist-mode 1)
  (show-paren-mode 1)
  (which-key-mode 1)
  (winner-mode 1)

  :hook
  ((prog-mode . display-line-numbers-mode)
   (prog-mode . electric-pair-local-mode)
   (prog-mode . hl-line-mode)
   (prog-mode . my--enable-delete-trailing-whitespace)
   (prog-mode . my--enable-show-trailing-whitespace)
   (prog-mode . visual-wrap-prefix-mode))

  :bind
  (([remap list-buffers] . ibuffer)
   ("C-c d" . duplicate-dwim)
   ("C-c m f" . my/find-file-with-sudo)
   ("C-c m s" . my/scratch-buffer-current-mode)
   ("C-c m w" . my/path-kill-ring-save)))

;;; Files and backups

(use-package files
  :preface
  (defvar my--backup-dir
    (expand-file-name "backup/" user-emacs-directory))
  (defvar my--auto-save-dir
    (expand-file-name "auto-save/" user-emacs-directory))
  (defun my--force-backup-of-buffer ()
    (setq buffer-backed-up nil))
  :init
  (make-directory my--backup-dir t)
  (make-directory my--auto-save-dir t)
  :custom
  (backup-by-copying t)
  (backup-directory-alist `(("." . ,my--backup-dir)))
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 0)
  (require-final-newline t)
  (version-control t)
  (auto-save-file-name-transforms `((".*" ,my--auto-save-dir t)))
  :hook
  (before-save 'my--force-backup-of-buffer))

;;; Tree-sitter modes

(use-package treesit
  :preface
  (defvar my--treesit-recipes
    '((bash
       :source ("https://github.com/tree-sitter/tree-sitter-bash" "v0.21.0")
       :remap (sh-mode . bash-ts-mode))
      (c
       :source ("https://github.com/tree-sitter/tree-sitter-c" "v0.21.4")
       :remap (c-mode . c-ts-mode))
      (cpp
       :source ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.3")
       :remap (c++-mode . c++-ts-mode))
      (css
       :source ("https://github.com/tree-sitter/tree-sitter-css" "v0.21.1")
       :remap (css-mode . css-ts-mode))
      (go
       :source ("https://github.com/tree-sitter/tree-sitter-go" "v0.21.2")
       :auto-mode ("\\.go\\'" . go-ts-mode))
      (html
       :source ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.3")
       :remap (mhtml-mode . html-ts-mode))
      (javascript
       :source ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.4")
       :remap (js-mode . js-ts-mode))
      (json
       :source ("https://github.com/tree-sitter/tree-sitter-json" "v0.21.0")
       :remap (js-json-mode . json-ts-mode))
      (python
       :source ("https://github.com/tree-sitter/tree-sitter-python" "v0.21.0")
       :remap (python-mode . python-ts-mode))
      (rust
       :source ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2")
       :auto-mode ("\\.rs\\'" . rust-ts-mode))
      (toml
       :source ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1")
       :auto-mode ("\\.toml\\'" . toml-ts-mode))
      (tsx
       :source ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2" "tsx/src")
       :auto-mode ("\\.tsx\\'" . tsx-ts-mode))
      (typescript
       :source ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2" "typescript/src")
       :auto-mode ("\\.ts\\'" . typescript-ts-mode)))
    "Tree-sitter language recipes for ABI version 14 (Emacs 31).
Each entry is (LANG . PLIST) where PLIST keys are:
  :source     (URL &optional REVISION SOURCE-DIR)
  :remap      (OLD-MODE . TS-MODE) for `major-mode-remap-alist'
  :auto-mode  (PATTERN . TS-MODE) for `auto-mode-alist'")

  (defun my--treesit-build-source-alist ()
    "Build `treesit-language-source-alist' from `my--treesit-recipes'."
    (mapcar (lambda (recipe)
              (cons (car recipe) (plist-get (cdr recipe) :source)))
            my--treesit-recipes))

  (defun my--treesit-configure-modes ()
    "Configure mode associations from `my--treesit-recipes'.
Only activates mappings for languages with installed grammars."
    (dolist (recipe my--treesit-recipes)
      (let ((lang (car recipe))
            (props (cdr recipe)))
	(when (treesit-language-available-p lang)
          (when-let* ((remap (plist-get props :remap)))
            (add-to-list 'major-mode-remap-alist remap))
          (when-let* ((entry (plist-get props :auto-mode)))
            (add-to-list 'auto-mode-alist entry)))))
    (when (and (treesit-language-available-p 'c)
               (treesit-language-available-p 'cpp))
      (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))))

  (defun my/treesit-install-all-grammars ()
    "Install all tree-sitter grammars from `my--treesit-recipes'."
    (interactive)
    (dolist (recipe my--treesit-recipes)
      (let ((lang (car recipe)))
	(unless (treesit-language-available-p lang)
          (treesit-install-language-grammar lang))))
    (my--treesit-configure-modes))

  :custom
  (treesit-font-lock-level 4)

  :config
  (setq treesit-language-source-alist (my--treesit-build-source-alist))
  (my--treesit-configure-modes))

;;; Dired

(use-package dired
  :preface
  (defvar my--dired-listing-switches
    (if (eq system-type 'gnu/linux)
	"-Alh --group-directories-first"
      "-Alh"))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches my--dired-listing-switches)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-mouse-drag-files t)
  (dired-free-space nil)
  :bind (:map dired-mode-map
              ("DEL" . dired-up-directory)))

;;; Diagnostics

(use-package flymake
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-suppress-zero-counters t)
  (flymake-wrap-around t))

;;; LSP

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t))

;;; Projects

(use-package project
  :custom
  (project-vc-extra-root-markers
   '(".project" "Makefile" "Cargo.toml" "go.mod" "package.json"
     "pyproject.toml" "CMakeLists.txt"))
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-dired "Dired")
     (project-any-command "Other")
     (project-compile "Compile" ?c)
     (project-shell "Shell" ?s)
     (magit-project-status "Magit" ?m))))

;;; Ef themes

(use-package ef-themes
  :ensure t
  :preface
  (defvar my/ef-themes-light 'ef-eagle
    "Default light theme.")
  (defvar my/ef-themes-dark 'ef-owl
    "Default dark theme.")
  (defun my/ef-themes--pair-member-p (theme)
    (memq theme (list my/ef-themes-light my/ef-themes-dark)))
  (defun my/ef-themes-toggle ()
    "Toggle between `my/ef-themes-dark' and `my/ef-themes-light'."
    (interactive)
    (let* ((current (car (seq-filter #'my/ef-themes--pair-member-p custom-enabled-themes)))
           (next (if (eq current my/ef-themes-dark) my/ef-themes-light my/ef-themes-dark)))
      (mapc #'disable-theme custom-enabled-themes)
      (modus-themes-load-theme next)))
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>"   . my/ef-themes-toggle)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t)
  (modus-themes-load-theme my/ef-themes-dark))

;;; Org

(use-package org
  :custom
  (org-directory "~/Documents/org")
  (org-return-follows-link t))

;;; Completion

(use-package vertico
  :ensure t
  :custom
  (vertico-count 24)
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-preselect 'first)
  :init
  (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-c r" . vertico-repeat)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.25 . 0.25))
  (corfu-popupinfo-max-height 12)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions
            (cape-capf-prefix-length #'cape-dabbrev 2)))

;;; Search and navigation

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
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
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))

;;; Actions

(use-package embark
  :ensure t
  :preface
  (defun my--embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (defun my--embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'my--embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  :custom
  (embark-indicators '(my--embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (advice-add #'embark-completing-read-prompter
              :around #'my--embark-hide-which-key-indicator)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Version control

(use-package magit
  :ensure t
  :commands (magit-status magit-blame-addition)
  :custom
  (magit-define-global-key-bindings 'default)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;;; ᗜˬᗜ

(provide 'init)

;;; init.el ends here
