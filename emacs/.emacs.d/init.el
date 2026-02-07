;; -*- lexical-binding: t; -*-

(use-package package
  :custom
  (package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)
  (load-theme 'modus-vivendi-tinted)
  (setq use-short-answers t
        create-lockfiles nil
        ring-bell-function 'ignore
        use-file-dialog nil)
  (setq scroll-preserve-screen-position t
	scroll-step 1
        scroll-conservatively 101))

(use-package faces
  :config
  (set-face-attribute 'default nil
                      :family "CommitMono"
                      :height 110))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook (prog-mode . display-line-numbers-mode))

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package visual-wrap
  :hook (after-init . global-visual-wrap-prefix-mode))

(use-package simple
  :preface
  (defun my--delete-trailing-whitespace ()
    (add-hook 'before-save-hook
              #'delete-trailing-whitespace
              nil
              t))
  :custom
  (blink-matching-paren t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :hook ((after-init . column-number-mode)
         (prog-mode . my--delete-trailing-whitespace)))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package mouse
  :hook (after-init . context-menu-mode))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t))

(use-package files
  :preface
  (defvar my--backup-dir
    (expand-file-name "backup/" user-emacs-directory))
  (defvar my--auto-save-dir
    (expand-file-name "auto-save/" user-emacs-directory))
  :init
  (make-directory my--backup-dir t)
  (make-directory my--auto-save-dir t)
  :custom
  (delete-by-moving-to-trash t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 0)
  (version-control t)
  (require-final-newline t)
  (backup-directory-alist `(("." . ,my--backup-dir)))
  (auto-save-file-name-transforms `((".*" ,my--auto-save-dir t)))
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (js-mode . js-ts-mode)
     (python-mode . python-ts-mode))))

(use-package cus-edit
  :custom
  (custom-file null-device))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package recentf
  :hook (after-init . recentf-mode))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package winner
  :hook (after-init . winner-mode))

(use-package repeat
  :hook (after-init . repeat-mode))

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :hook (after-init . which-key-mode))

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-agho --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)))

(use-package dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode))

(use-package org
  :custom
  (org-directory "~/Documents/org")
  (org-return-follows-link t))

(use-package treesit
  :custom
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (python "https://github.com/tree-sitter/tree-sitter-python"))))

(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 24)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)
  (completion-styles '(orderless basic)))

(use-package consult
  :ensure t
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("C-c m" . consult-mode-command)
         ("C-c h" . consult-history))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq completion-in-region-function #'consult-completion-in-region
        register-preview-delay 0.5
        xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
