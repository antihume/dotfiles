;; -*- lexical-binding: t; -*-

;;; Personal preferences

(defgroup my nil
  "Personal Emacs configuration preferences."
  :group 'convenience)

(defcustom my/theme-light 'modus-operandi
  "Default light theme."
  :type 'symbol
  :group 'my)

(defcustom my/theme-dark 'modus-vivendi
  "Default dark theme."
  :type 'symbol
  :group 'my)

(defvar my/theme-current my/theme-dark
  "Current theme variant, either `my/theme-light' or `my/theme-dark'.")

(defcustom my/font-family "CommitMono"
  "Default font family."
  :type 'string
  :group 'my)

(defcustom my/font-height 110
  "Default font height in 1/10pt units."
  :type 'integer
  :group 'my)

;;; Helper functions

(defun my/theme-load (theme)
  "Disable enabled themes and load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (setq my/theme-current theme))

(defun my/theme-toggle ()
  "Toggle between `my/theme-dark' and `my/theme-light'."
  (interactive)
  (my/theme-load
   (if (eq my/theme-current my/theme-dark)
       my/theme-light
     my/theme-dark)))

(defun my/open-file-with-sudo ()
  "Reopen the current file with sudo privileges."
  (interactive)
  (if-let* ((file-name (buffer-file-name)))
      (if (file-remote-p file-name)
          (user-error "File is already remote")
        (find-alternate-file (concat "/sudo::" file-name)))
    (user-error "Buffer is not visiting a file")))

(defun my/save-path-to-kill-ring ()
  "Save current file path to kill ring."
  (interactive)
  (if-let* ((filename (buffer-file-name)))
      (progn
        (kill-new filename)
        (message "Copied: %s" filename))
    (user-error "Buffer is not visiting a file")))

;;; Tree-sitter

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
  (defun my--enable-show-trailing-whitespace ()
    (setq-local show-trailing-whitespace t))

  (defun my--enable-delete-trailing-whitespace ()
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  :custom

  ;; General behavior
  (create-lockfiles nil)
  (custom-file null-device)
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
  (which-key-idle-delay 1)

  ;; Windmove
  (windmove-default-keybindings 'meta)

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

  ;; Modus Themes
  (require-theme 'modus-themes)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-common-palette-overrides
        modus-themes-preset-overrides-warmer)

  ;; Theme
  (my/theme-load my/theme-current)

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

  :hook
  ((prog-mode . display-line-numbers-mode)
   (prog-mode . electric-pair-local-mode)
   (prog-mode . hl-line-mode)
   (prog-mode . my--enable-delete-trailing-whitespace)
   (prog-mode . my--enable-show-trailing-whitespace)
   (prog-mode . visual-wrap-prefix-mode))

  :bind
  (([remap list-buffers] . ibuffer)
   ("<f5>" . my/theme-toggle)
   ("C-c m s" . my/open-file-with-sudo)
   ("C-c m y" . my/save-path-to-kill-ring)
   ("C-c d" . duplicate-dwim)))

;;; Files and backups

(defvar my--backup-dir
  (expand-file-name "backup/" user-emacs-directory))

(defvar my--auto-save-dir
  (expand-file-name "auto-save/" user-emacs-directory))

(make-directory my--backup-dir t)
(make-directory my--auto-save-dir t)

(use-package files
  :custom
  (delete-by-moving-to-trash t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 0)
  (version-control t)
  (require-final-newline t)
  (backup-directory-alist `(("." . ,my--backup-dir)))
  (auto-save-file-name-transforms `((".*" ,my--auto-save-dir t))))

;;; Tree-sitter modes

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist (my--treesit-build-source-alist))
  (my--treesit-configure-modes))

;;; Dired

(defvar my--dired-listing-switches
  (if (eq system-type 'gnu/linux)
      "-agho --group-directories-first"
    "-agho"))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-listing-switches my--dired-listing-switches)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-mouse-drag-files t)
  (dired-free-space nil)
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)))

;;; Windows

(use-package winner
  :config
  (winner-mode 1)
  :bind
  (("C-c w u" . winner-undo)
   ("C-c w r" . winner-redo)))

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
  (corfu-auto-delay 0.75)
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
  :ensure t
  :custom
  (consult-narrow-key "<")
  (consult-grep-args
   '("grep" (consult--grep-exclude-args)
     "--null --line-buffered --color=never --ignore-case \
--with-filename --line-number -I -r"))
  (consult-find-args
   "find . -type f \
-not ( -path '*/.git/*' -prune ) \
-not ( -path '*/node_modules/*' -prune ) \
-not ( -path '*/.cache/*' -prune )")
  :init
  (setq register-preview-delay 0.5
        xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ("M-s f" . consult-find)
   ("M-s g" . consult-grep)
   ("M-s l" . consult-line)))

;;; Actions

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
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
  :bind (("C-c g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(provide 'init)

;;; init.el ends here
