;; -*- lexical-binding: t; -*-

(use-package package
  :custom
  (package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq use-short-answers t
        create-lockfiles nil
        ring-bell-function 'ignore
        use-file-dialog nil)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-common-palette-overrides
        modus-themes-preset-overrides-faint)
  (load-theme 'modus-vivendi))

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
  :hook ((after-init . column-number-mode)
         (prog-mode . my--delete-trailing-whitespace)))

(use-package delsel
  :hook (after-init . delete-selection-mode))

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
  (which-key-idle-delay 1)
  :hook (after-init . which-key-mode))

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package icomplete
  :hook (after-init . fido-vertical-mode))
(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-agho --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)))

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

(use-package meow
  :ensure t
  :demand t
  :preface
  (defun my--meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config
  (my--meow-setup)
  (meow-global-mode 1))

(use-package magit
  :defer t
  :commands (magit-status magit-dispatch))
