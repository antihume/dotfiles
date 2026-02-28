;; ᗜˬᗜ -*- lexical-binding: t; -*-

(use-package package
  :custom
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                      ("melpa" . "https://melpa.org/packages/")))
  (package-archive-priorities '(("gnu" . 3)
                                ("nongnu" . 2)
                                ("melpa" . 1))))

(use-package emacs
  :preface
  (defun my--enable-show-trailing-whitespace ()
    (setq-local show-trailing-whitespace t))

  (defun my--enable-delete-trailing-whitespace ()
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  (defun my/find-file-with-sudo ()
    "Find file as root via TRAMP sudo."
    (interactive)
    (if (file-remote-p default-directory)
        (user-error "Cannot sudo on a remote path")
      (let* ((base-dir (or (and-let* ((file (buffer-file-name)))
                             (file-name-directory file))
                           default-directory
                           "/"))
             (default-directory
              (concat "/sudo::" (file-name-as-directory (expand-file-name base-dir)))))
        (call-interactively #'find-file))))

  (defun my/path-kill-ring-save ()
    "Save current file or Dired directory path to kill ring."
    (interactive)
    (if-let* ((filename (or (buffer-file-name)
                            (and (derived-mode-p 'dired-mode)
                                 (dired-current-directory)))))
        (progn
          (kill-new filename)
          (message "Copied: %s" filename))
      (user-error "Buffer is not visiting a file or Dired directory")))

  (defun my/scratch-buffer-current-mode ()
    "Switch to a scratch buffer in the current major mode."
    (interactive)
    (let* ((mode major-mode)
           (name (format "*scratch-%s*" mode))
           (buf (get-buffer-create name)))
      (switch-to-buffer buf)
      (unless (eq major-mode mode)
        (funcall mode))))

  (defun my/narrow-or-widen-dwim (p)
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
          ((and (derived-mode-p 'latex-mode)
                (fboundp 'LaTeX-narrow-to-environment))
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))

  :custom

  ;; Prompts & Dialogs
  (confirm-kill-emacs #'yes-or-no-p)
  (disabled-command-function nil)
  (ring-bell-function 'ignore)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)

  ;; Files & Sessions
  (bookmark-save-flag 1)
  (create-lockfiles nil)
  (custom-file null-device)
  (find-file-visit-truename t)
  (recentf-max-saved-items 200)
  (vc-follow-symlinks t)

  ;; Editing
  (delete-trailing-whitespace-exclude-patterns '("^//.*" "^#.*" "^--.*" "^/*.*"))
  (indent-tabs-mode nil)
  (kill-whole-line t)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (text-mode-ispell-word-completion nil)

  ;; Kill Ring & Clipboard
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)

  ;; Minibuffer & Completion
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Scrolling & Performance
  (auto-window-vscroll nil)
  (fast-but-imprecise-scrolling t)
  (pixel-scroll-precision-use-momentum t)
  (redisplay-skip-fontification-on-input t)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)

  ;; Display & Windows
  (display-line-numbers-type t)
  (fill-column 80)
  (mode-line-compact 'long)
  (word-wrap t)
  (x-stretch-cursor t)

  ;; Help & Discovery
  (apropos-do-all t)
  (describe-bindings-outline t)
  (help-window-select t)
  (which-key-idle-delay 0.375)

  ;; Buffer Naming
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/")

  ;; Calculator
  (calc-group-char ",")
  (calc-group-digits t)

  :config

  ;; Font
  (when (member "Iosevka SS12" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Iosevka SS12"
                        :height 120))

  ;; Global Modes
  (column-number-mode)
  (context-menu-mode)
  (delete-selection-mode)
  (editorconfig-mode)
  (global-auto-revert-mode)
  (global-goto-address-mode)
  (global-so-long-mode)
  (minibuffer-depth-indicate-mode)
  (pixel-scroll-precision-mode)
  (recentf-mode)
  (repeat-mode)
  (save-place-mode)
  (which-key-mode)

  :hook
  ((imenu-after-jump . my--pulse-line-after-jump)
   (prog-mode . display-line-numbers-mode)
   (prog-mode . electric-pair-local-mode)
   (prog-mode . hl-line-mode)
   (prog-mode . my--enable-delete-trailing-whitespace)
   (prog-mode . my--enable-show-trailing-whitespace)
   (prog-mode . visual-wrap-prefix-mode)
   (xref-after-jump . my--pulse-line-after-jump))

  :bind
  (("C-c D" . diff-buffer-with-file)
   ("C-c d" . duplicate-dwim)
   ("C-c f" . my/find-file-with-sudo)
   ("C-c j" . join-line)
   ("C-c m" . my/narrow-or-widen-dwim)
   ("C-c p" . my/path-kill-ring-save)
   ("C-c R" . ff-find-other-file)
   ("C-c s" . my/scratch-buffer-current-mode)
   ("C-x k" . kill-current-buffer)
   ("M-c" . capitalize-dwim)
   ("M-l" . downcase-dwim)
   ("M-u" . upcase-dwim)
   ("M-z" . zap-up-to-char)))

(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (lazy-highlight-initial-delay 0))

(use-package compile
  :defer t
  :bind
  (("C-c c" . recompile)
   ("C-c C" . compile))
  :custom
  (compilation-always-kill t)
  (compilation-auto-jump-to-first-error 'if-location-known)
  (compilation-scroll-output 'first-error)
  (next-error-message-highlight t))

(use-package pulse
  :preface
  (defface my--pulse-face
    '((t :inherit highlight :extend t))
    "My face for pulse highlights.")
  (defun my--pulse-line-after-jump (&rest _)
    (pulse-momentary-highlight-one-line (point) 'my--pulse-face))
  :custom
  (pulse-delay 0.03125)
  (pulse-iterations 16)
  :config
  (dolist (cmd '(avy-goto-end-of-line
                 avy-resume
                 avy-goto-word-1
                 avy-goto-char-timer
                 avy-isearch))
    (advice-add cmd :after #'my--pulse-line-after-jump))
  (advice-add #'my/other-window-mru-or-split :after #'my--pulse-line-after-jump)
  (advice-add #'other-window :after #'my--pulse-line-after-jump)
  (advice-add #'pop-global-mark :after #'my--pulse-line-after-jump)
  (advice-add #'pop-to-mark-command :after #'my--pulse-line-after-jump))


(use-package window
  :preface
  (defalias 'my/other-window-mru-or-split
    (let ((windows nil) (index 0))
      (lambda ()
        "Cycle windows in most-recently-used order.
Freeze order on first call; advance on repeat; reset otherwise.
If there is only one window, split it sensibly first."
        (interactive)
        (when (one-window-p t)
          (split-window-sensibly))
        (if (eq last-command 'my/other-window-mru-or-split)
            (setq index (mod (1+ index) (length windows)))
          (select-window (selected-window))
          (setq windows (sort (window-list nil 'nomini)
                              :key #'window-use-time :reverse t)
                index 1))
        (select-window (nth index windows) 'norecord))))
  :bind
  (("C-x o" . my/other-window-mru-or-split)
   :map other-window-repeat-map
   ("o" . my/other-window-mru-or-split))
  :custom
  (window-combination-resize t)
  :config
  (put 'my/other-window-mru-or-split 'repeat-map 'other-window-repeat-map))

(use-package files
  :preface
  (defun my--force-backup-of-buffer ()
    (setq buffer-backed-up nil))
  :init
  (make-directory (expand-file-name "backup/" user-emacs-directory) t)
  (make-directory (expand-file-name "auto-save/" user-emacs-directory) t)
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
  (backup-by-copying t)
  (backup-directory-alist
   `(("." . ,(expand-file-name "backup/" user-emacs-directory))))
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (kept-new-versions 8)
  (kept-old-versions 2)
  (require-final-newline t)
  (version-control t)
  :hook
  (before-save . my--force-backup-of-buffer))

(use-package treesit
  :preface
  (defconst my--treesit-recipes
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
    (when (treesit-available-p)
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
        (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))))

  (defun my/treesit-install-all-grammars ()
    "Install all tree-sitter grammars from `my--treesit-recipes'."
    (interactive)
    (unless (treesit-available-p)
      (user-error "This Emacs was built without tree-sitter support"))
    (dolist (recipe my--treesit-recipes)
      (let ((lang (car recipe)))
        (unless (treesit-language-available-p lang)
          (treesit-install-language-grammar lang))))
    (my--treesit-configure-modes))

  :custom
  (treesit-font-lock-level 4)

  :config
  (when (treesit-available-p)
    (setq treesit-language-source-alist (my--treesit-build-source-alist))
    (my--treesit-configure-modes)))

(use-package savehist
  :custom
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring compile-command corfu-history))
  :config
  (savehist-mode))

(use-package paren
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen 'overlay)
  :config
  (show-paren-mode))

(use-package dired
  :preface
  (defconst my--dired-listing-switches
    (if (eq system-type 'gnu/linux)
        "-AGFhlv --group-directories-first"
      "-AohF"))
  :custom
  (dired-dwim-target t)
  (dired-free-space nil)
  (dired-listing-switches my--dired-listing-switches)
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :bind (:map dired-mode-map
              ("DEL" . dired-up-directory))
  :hook
  (dired-mode . hl-line-mode)
  (dired-mode . dired-hide-details-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :preface
  (defun my--ibuffer-init ()
    (ibuffer-switch-to-saved-filter-groups "Default"))
  :custom
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-expert t)
  (ibuffer-old-time 48)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Org"       (mode . org-mode))
      ("Code"      (and (or (derived-mode . prog-mode)
                            (derived-mode . conf-mode))
                        (not (name . "^\\*"))))
      ("Compilation" (and (derived-mode . compilation-mode)
                          (not (name . "^\\*"))))
      ("Text"      (derived-mode . text-mode))
      ("Document"  (or (mode . pdf-view-mode)
                       (mode . doc-view-mode)
                       (mode . image-mode)))
      ("Web"       (mode . eww-mode))
      ("Dired"     (mode . dired-mode))
      ("Version Control" (or (derived-mode . magit-mode)
                             (derived-mode . vc-dir-mode)
                             (mode . diff-mode)))
      ("Terminal"  (or (mode . vterm-mode)
                       (mode . eshell-mode)
                       (mode . term-mode)
                       (mode . shell-mode)))
      ("Help"      (or (derived-mode . help-mode)
                       (derived-mode . Info-mode)
                       (derived-mode . apropos-mode)
                       (derived-mode . Custom-mode)
                       (mode . Man-mode)
                       (mode . woman-mode)))
      ("Special"   (name . "^\\*")))))
  :hook
  (ibuffer-mode . my--ibuffer-init)
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1048576) (format "%7.1fM" (/ (buffer-size) 1048576.0)))
     ((> (buffer-size) 1024)    (format "%7.1fk" (/ (buffer-size) 1024.0)))
     (t                         (format "%8d"      (buffer-size)))))
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))

(use-package flymake
  :defer t
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-suppress-zero-counters t)
  (flymake-wrap-around t))

(use-package eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 20000)
  (eglot-extend-to-xref t))

(use-package project
  :defer t
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
     (project-vterm "Terminal" ?t)
     (magit-project-status "Magit" ?m))))

(use-package tab-bar
  :bind
  (("C-c [" . tab-bar-history-back)
   ("C-c ]" . tab-bar-history-forward)
   :map tab-bar-history-mode-map
   ("C-c <left>" . nil)
   ("C-c <right>" . nil)
   :map tab-bar-history-repeat-map
   ("<left>" . nil)
   ("<right>" . nil)
   ("[" . tab-bar-history-back)
   ("]" . tab-bar-history-forward))
  :custom
  (tab-bar-auto-width t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice #'scratch-buffer)
  :config
  (tab-bar-history-mode)
  (tab-bar-mode))

(use-package ef-themes
  :ensure t
  :preface
  (defconst my/ef-themes-light 'ef-reverie
    "Default light theme.")
  (defconst my/ef-themes-dark 'ef-dream
    "Default dark theme.")
  (defun my--ef-themes-pair-member-p (theme)
    (memq theme (list my/ef-themes-light my/ef-themes-dark)))
  (defun my/ef-themes-toggle ()
    "Toggle between `my/ef-themes-dark' and `my/ef-themes-light'."
    (interactive)
    (let* ((current (car (seq-filter #'my--ef-themes-pair-member-p custom-enabled-themes)))
           (next (if (eq current my/ef-themes-dark) my/ef-themes-light my/ef-themes-dark)))
      (mapc #'disable-theme custom-enabled-themes)
      (modus-themes-load-theme next)))
  :init
  (ef-themes-take-over-modus-themes-mode)
  :bind
  (("<f5>"   . my/ef-themes-toggle)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-italic-constructs t)
  :config
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (modus-themes-load-theme my/ef-themes-dark))

(use-package org
  :defer t
  :custom
  (org-directory "~/org")
  (org-highlight-latex-and-related '(native latex script entities))
  (org-preview-latex-default-process 'dvisvgm)
  (org-return-follows-link t)
  :config
  (plist-put org-format-latex-options :scale 2))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 24)
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-preselect 'first)
  :init
  (vertico-mode))

(use-package vertico-multiform
  :after vertico
  :custom
  (vertico-multiform-categories
   '((file (:keymap . vertico-directory-map))
     (consult-grep buffer (vertico-cycle . nil))
     (consult-location buffer (vertico-cycle . nil))))
  :init
  (vertico-multiform-mode))

(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
              ("M-j" . vertico-quick-exit)))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  ("C-c r" . vertico-repeat))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("<remap> <next-line>" . nil)
        ("<remap> <previous-line>" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1875)
  (corfu-auto-prefix 4)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :bind ("M-+" . cape-prefix-map)
  :preface
  (defun my--cape-setup-capf ()
    (add-hook 'completion-at-point-functions #'cape-file t t)
    (add-hook 'completion-at-point-functions #'cape-elisp-block t t)
    (add-hook 'completion-at-point-functions #'cape-dabbrev t t))
  :hook
  ((prog-mode . my--cape-setup-capf)
   (conf-mode . my--cape-setup-capf)
   (text-mode . my--cape-setup-capf)
   (comint-mode . my--cape-setup-capf)
   (eshell-mode . my--cape-setup-capf)))

(use-package consult
  :ensure t
  :bind (("C-c M-x" . consult-mode-command)
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
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.5 any))
  (setq consult-narrow-key "C-+")
  :hook
  (consult-after-jump . my--pulse-line-after-jump))

(use-package avy
  :ensure t
  :demand t
  :preface
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (defun avy-goto-char-flash (&optional arg)
    (interactive "P")
    (avy-with avy-goto-char-flash
      (let* ((str "")
             (avy-all-windows (if arg (not avy-all-windows) avy-all-windows))
             (alphabet (append "asdfghjklqwertyuiopzxcvbnm\
ASDFGHJKLQWERTYUIOPZXCVBNM" nil))
             (used-map (make-hash-table :test 'equal))
             sorted key-to-cand flash-ovs res)
        (cl-labels ((cand-id (cand)
                      (list (cdr cand) (caar cand)))
                    (reusable-label-p (label)
                      (= label (downcase label))))
          (unwind-protect
              (progn
                (avy--make-backgrounds (avy-window-list))
                (setq res
                      (catch 'done
                        (while t
                          (mapc #'delete-overlay flash-ovs)
                          (setq flash-ovs nil sorted nil key-to-cand nil)
                          (when (> (length str) 0)
                            (let ((candidates (avy--regex-candidates
                                               (regexp-quote str))))
                              (if (null candidates)
                                  (beep)
                                (let* ((case-fold
                                        (or avy-case-fold-search
                                            (string= str (downcase str))))
                                       (unsafe (make-hash-table :test 'equal))
                                       (safe
                                        (let ((re (regexp-quote str))
                                              (bufs (make-hash-table :test 'eq)))
                                          (dolist (w (avy-window-list))
                                            (let ((buf (window-buffer w)))
                                              (unless (gethash buf bufs)
                                                (puthash buf t bufs)
                                                (with-current-buffer buf
                                                  (let ((case-fold-search case-fold))
                                                    (save-excursion
                                                      (goto-char (point-min))
                                                      (while (re-search-forward re nil t)
                                                        (let ((ch (char-after)))
                                                          (when ch
                                                            (puthash
                                                             (if case-fold (downcase ch) ch)
                                                             t unsafe))))))))))
                                          (cl-remove-if
                                           (lambda (label)
                                             (gethash (if case-fold
                                                          (downcase label)
                                                        label)
                                                      unsafe))
                                           alphabet)))
                                       (wnd (selected-window))
                                       (pt (point))
                                       (window-rank (let ((ht (make-hash-table :test 'eq))
                                                          (rank 1))
                                                      (dolist (w (avy-window-list) ht)
                                                        (unless (eq w wnd)
                                                          (puthash w rank ht)
                                                          (setq rank (1+ rank)))))))
                                  (setq sorted
                                        (sort (copy-sequence candidates)
                                              (lambda (a b)
                                                (let* ((wa (cdr a))
                                                       (wb (cdr b))
                                                       (a-cur (eq wa wnd))
                                                       (b-cur (eq wb wnd)))
                                                  (cond
                                                   ((and a-cur (not b-cur)) t)
                                                   ((and (not a-cur) b-cur) nil)
                                                   ((and a-cur b-cur)
                                                    (let ((da (abs (- (caar a) pt)))
                                                          (db (abs (- (caar b) pt))))
                                                      (if (= da db)
                                                          (< (caar a) (caar b))
                                                        (< da db))))
                                                   (t
                                                    (let ((ra (or (gethash wa window-rank)
                                                                  most-positive-fixnum))
                                                          (rb (or (gethash wb window-rank)
                                                                  most-positive-fixnum)))
                                                      (if (= ra rb)
                                                          (< (caar a) (caar b))
                                                        (< ra rb)))))))))
                                  (let ((taken (make-hash-table :test 'eq))
                                        (assigned (make-hash-table :test 'equal))
                                        labeled)
                                    (dolist (cand sorted)
                                      (let* ((id (cand-id cand))
                                             (prev (gethash id used-map)))
                                        (when (and prev
                                                   (memq prev safe)
                                                   (not (gethash prev taken)))
                                          (puthash prev t taken)
                                          (puthash id prev assigned)
                                          (push (cons cand prev) labeled))))
                                    (let ((avail (cl-remove-if
                                                  (lambda (label)
                                                    (gethash label taken))
                                                  safe)))
                                      (dolist (cand sorted)
                                        (let ((id (cand-id cand)))
                                          (unless (gethash id assigned)
                                            (let ((label (pop avail)))
                                              (when label
                                                (puthash label t taken)
                                                (puthash id label assigned)
                                                (push (cons cand label) labeled)))))))
                                    (setq labeled (nreverse labeled)
                                          key-to-cand nil)
                                    (dolist (lc labeled)
                                      (let* ((cand (car lc))
                                             (label (cdr lc)))
                                        (when (reusable-label-p label)
                                          (puthash (cand-id cand) label used-map))
                                        (push (cons label cand) key-to-cand)))
                                    (setq key-to-cand (nreverse key-to-cand)))
                                  (when (and (= (length sorted) 1)
                                             avy-single-candidate-jump)
                                    (throw 'done (car sorted)))
                                  (dolist (cand sorted)
                                    (let ((ov (make-overlay
                                               (caar cand) (cdar cand)
                                               (window-buffer (cdr cand)))))
                                      (overlay-put ov 'face 'lazy-highlight)
                                      (overlay-put ov 'window (cdr cand))
                                      (overlay-put ov 'priority 0)
                                      (push ov flash-ovs)))
                                  (dolist (lc key-to-cand)
                                    (let* ((cand (cdr lc))
                                           (end (cdar cand))
                                           (w (cdr cand))
                                           (buf (window-buffer w))
                                           (lbl (propertize (string (car lc))
                                                            'face 'avy-lead-face))
                                           (ov (with-current-buffer buf
                                                 (cond
                                                  ((>= end (point-max))
                                                   (let ((o (make-overlay
                                                             (1- end) end buf)))
                                                     (overlay-put o 'after-string lbl)
                                                     o))
                                                  ((eq (char-after end) ?\n)
                                                   (let ((o (make-overlay
                                                             end (1+ end) buf)))
                                                     (overlay-put
                                                      o 'display
                                                      (concat lbl "\n"))
                                                     o))
                                                  (t
                                                   (let ((o (make-overlay
                                                             end (1+ end) buf)))
                                                     (overlay-put o 'display lbl)
                                                     o))))))
                                      (push ov flash-ovs)))))))
                          (let ((char (read-char
                                       (if (string= str "")
                                           "char: "
                                         (format "%d (%s): "
                                                 (length sorted) str))
                                       t)))
                            (cond
                             ((memq char '(27 ?\C-g))
                              (throw 'done nil))
                             ((= char 13)
                              (throw 'done (car sorted)))
                             ((memq char avy-del-last-char-by)
                              (when (> (length str) 0)
                                (setq str (substring str 0 -1))))
                             ((cdr (assq char key-to-cand))
                              (throw 'done (cdr (assq char key-to-cand))))
                             (t
                              (setq str (concat str (string char))))))))))
            (mapc #'delete-overlay flash-ovs)
            (avy--done))
          (when res
            (funcall avy-pre-action res)
            (funcall (or avy-action #'avy-action-goto)
                     (if (consp (car res)) (caar res) (car res))))))))
  :bind (("M-g l" . avy-goto-end-of-line)
         ("M-g r" . avy-resume)
         ("M-g w" . avy-goto-word-1)
         ("M-j"   . avy-goto-char-flash)
         :map isearch-mode-map
         ("M-j"   . avy-isearch))
  :custom
  (avy-all-windows t)
  (avy-background t)
  (avy-keys '(?q ?w ?e ?d ?p ?o ?k ?j ?l))
  (avy-style 'at-full)
  (avy-timeout-seconds 0.25)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package embark
  :ensure t
  :preface
  (defun my--embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key popup is delayed by `which-key-idle-delay'."
    (let ((timer nil))
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (progn
              (when timer (cancel-timer timer) (setq timer nil))
              (which-key--hide-popup-ignore-command))
          (when timer (cancel-timer timer))
          (setq timer
                (run-with-idle-timer
                 which-key-idle-delay nil
                 (lambda ()
                   (setq timer nil)
                   (which-key--show-keymap
                    (if (eq (plist-get (car targets) :type) 'embark-become)
                        "Become"
                      (format "Act on %s '%s'%s"
                              (plist-get (car targets) :type)
                              (embark--truncate-target (plist-get (car targets) :target))
                              (if (cdr targets) " " "")))
                    (if prefix
                        (pcase (lookup-key keymap prefix 'accept-default)
                          ((and (pred keymapp) km) km)
                          (_ (key-binding prefix 'accept-default)))
                      keymap)
                    nil nil t (lambda (binding)
                                (not (string-suffix-p "-argument" (cdr binding))))))))))))
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
   ("M-." . embark-dwim)
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
  :after (embark consult))

(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package vterm
  :ensure t
  :preface
  (defun project-vterm ()
    "Open vterm in the current project root, or the current directory."
    (interactive)
    (let* ((root (or (when-let* ((proj (project-current)))
                       (project-root proj))
                     default-directory))
           (name (format "*vterm %s*" (abbreviate-file-name root)))
           (buf (get-buffer name)))
      (if buf
          (switch-to-buffer buf)
        (let ((default-directory root))
          (vterm name)))))
  :custom
  (vterm-always-compile-module t)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-timer-delay nil)
  :bind
  (("C-c t" . vterm)
   ("C-c T" . project-vterm)
   :map vterm-mode-map
   ("C-q" . vterm-send-next-key)))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 1.125)
  (pdf-view-use-imagemagick nil)
  (pdf-view-use-scaling t)
  :bind
  (:map pdf-view-mode-map
        ("<f6>" . pdf-view-midnight-minor-mode))
  :config
  (pdf-loader-install))

;;; ᗜˬᗜ

(provide 'init)

;;; init.el ends here
