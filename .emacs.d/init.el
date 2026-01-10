;;; init.el --- Personal configuration -*- lexical-binding: t -*-

;; --- Package setup ----------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; --- Cleaner interface ------------------------------------------------------
(fringe-mode 0)
(setopt inhibit-startup-screen t)
(setopt initial-scratch-message nil)
(setopt ring-bell-function 'ignore)

;; --- Better defaults --------------------------------------------------------
(setopt use-short-answers t)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(setopt select-enable-clipboard t)
(set-default-coding-systems 'utf-8)
(setopt indent-tabs-mode nil)
(setopt apropos-do-all t)
(setopt require-final-newline t)

;; --- Line numbers -----------------------------------------------------------
(global-display-line-numbers-mode 1)
(setopt display-line-numbers-type 'relative)
(column-number-mode 1)

;; --- Buffer tabs ------------------------------------------------------------
(global-tab-line-mode 1)
(setopt tab-line-close-button-show nil)
(setopt tab-line-new-button-show nil)

;; --- Scrolling --------------------------------------------------------------
(pixel-scroll-precision-mode 1)
(setopt pixel-scroll-precision-interpolate-page t)
(setopt scroll-margin 0
        scroll-preserve-screen-position nil
        scroll-step 1)

;; --- Visual tweaks ----------------------------------------------------------
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; --- Show matching parens ---------------------------------------------------
(show-paren-mode 1)
(setopt show-paren-delay 0)

;; --- Backups out of the way -------------------------------------------------
(setopt backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setopt auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; --- Auto revert (sync with disk) -------------------------------------------
(global-auto-revert-mode 1)
(setopt auto-revert-verbose nil)
(setopt auto-revert-check-vc-info t)
(setopt global-auto-revert-non-file-buffers t)

;; --- Remember state ---------------------------------------------------------
(save-place-mode 1)
(setopt save-place-forget-unreadable-files t)

;; --- Handle long lines gracefully -------------------------------------------
(global-so-long-mode 1)

;; --- Unique buffer names ----------------------------------------------------
(setopt uniquify-buffer-name-style 'forward)

;; --- Window management ------------------------------------------------------
(winner-mode 1)
(repeat-mode 1)
(windmove-default-keybindings)

;; --- Custom file (keep init.el clean) ---------------------------------------
(setopt custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; --- Keybinding fixes -------------------------------------------------------
(keymap-global-set "C-x k" #'kill-current-buffer)
(keymap-global-unset "C-z")
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

;; --- Font -------------------------------------------------------------------
(set-face-attribute 'default nil
                    :family "Iosevka Fixed SS08"
                    :height 120)

;; --- Theme ------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'modus-operandi-tinted t)

;; --- Meow (modal editing) ---------------------------------------------------
(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
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
  (meow-setup)
  (meow-global-mode 1))

;; --- Which-key (discoverability) --------------------------------------------
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

;; --- Completion stack -------------------------------------------------------
(use-package vertico
  :init
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :init
  (setq consult-async-min-input 2)
  :config
  (meow-leader-define-key
   '("." . consult-find)
   '("/" . consult-grep)
   '("b" . consult-buffer)
   '("l" . consult-line)
   '("o" . consult-outline)
   '("r" . consult-recent-file)
   '("i" . consult-imenu)))

(use-package corfu
  :init
  (global-corfu-mode 1)
  (setq corfu-auto t
        corfu-auto-delay 0.25
        corfu-auto-prefix 3
        corfu-cycle t
        corfu-preselect 'prompt)
  :config
  (corfu-popupinfo-mode 1))

;; --- vterm (Terminal) -------------------------------------------------------
(use-package vterm
  :config
  (setq vterm-max-scrollback 10000)
  :hook
  (vterm-mode . (lambda ()
                  (display-line-numbers-mode -1)
                  (hl-line-mode -1))))

;; --- Dired ------------------------------------------------------------------
(use-package dired
  :ensure nil
  :init
  (setq dired-listing-switches "-alh --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-kill-when-opening-new-dired-buffer t)
  :config
  (meow-leader-define-key
   '("d" . dired-jump)))

;; --- Eglot (LSP) ------------------------------------------------------------
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0))

;; --- Flymake (diagnostics) --------------------------------------------------
(use-package flymake
  :ensure nil
  :hook (eglot-managed-mode . flymake-mode)
  :config
  (meow-leader-define-key
   '("e" . flymake-show-buffer-diagnostics)))

;; --- Magit (git interface) --------------------------------------------------
(use-package magit)

;; --- pdf-tools (PDF reader) -------------------------------------------------
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("h" . image-backward-hscroll)
              ("l" . image-forward-hscroll))
  :init
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-continuous t
        pdf-annot-activate-created-annotations t
        pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

;; --- Treesit (tree-sitter) --------------------------------------------------
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")))

;; Prefer tree-sitter modes
(setopt major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (rust-mode . rust-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)))

;;; init.el ends here
