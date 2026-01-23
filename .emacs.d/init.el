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
        modus-themes-bold-constructs t)
  (load-theme 'modus-vivendi :no-confirm))

(use-package faces
  :config
  (set-face-attribute 'default nil
                      :family "CommitMono Nerd Font Mono"
                      :height 110))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook (prog-mode . display-line-numbers-mode))

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(use-package simple
  :custom
  (blink-matching-paren t)
  :hook ((after-init . column-number-mode)
         (before-save . delete-trailing-whitespace))
  :config
  (delete-selection-mode 1))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (use-short-answers t))

(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package visual-wrap
  :hook (after-init . global-visual-wrap-prefix-mode))

(use-package files
  :preface
  (defvar my--backup-dir
    (expand-file-name "backup/" user-emacs-directory))
  (defvar my--auto-save-dir
    (expand-file-name "auto-save/" user-emacs-directory))
  :custom
  (create-lockfiles nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 0)
  (version-control t)
  (require-final-newline t)
  (backup-directory-alist `(("." . ,my--backup-dir)))
  (auto-save-file-name-transforms `((".*" ,my--auto-save-dir t)))
  :init
  (make-directory my--backup-dir t)
  (make-directory my--auto-save-dir t))

(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory)))

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
  (which-key-idle-delay 0.3)
  :hook (after-init . which-key-mode))

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package dired
  :custom
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-listing-switches "-lhv --group-directories-first")
  (dired-recursive-copies 'always)
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)))

(use-package icomplete
  :hook (after-init . fido-vertical-mode))

(use-package treesit
  :custom
  (treesit-language-source-alist
   '((bash   "https://github.com/tree-sitter/tree-sitter-bash")
     (c      "https://github.com/tree-sitter/tree-sitter-c")
     (cpp    "https://github.com/tree-sitter/tree-sitter-cpp")
     (js     "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (python "https://github.com/tree-sitter/tree-sitter-python")))
  (major-mode-remap-alist
   '((bash-mode   . bash-ts-mode)
     (c-mode      . c-ts-mode)
     (c++-mode    . c++-ts-mode)
     (js-mode     . js-ts-mode)
     (python-mode . python-ts-mode))))

