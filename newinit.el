;;; newinit.el --- Modern Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A modernized Emacs configuration focusing on:
;; - Evil mode for Vim-like editing
;; - Development tools (LSP, Company, etc.)
;; - Clojure development
;; - Web development
;; - Clean UI and better defaults

;;; Code:

;; Performance tweaks
(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))

;; Package system setup
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Core UI improvements
(use-package emacs
  :init
  ;; UI elements
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode 0)
  (global-display-line-numbers-mode)
  (column-number-mode 1)
  
  ;; Better defaults
  (setq inhibit-startup-message t
        create-lockfiles nil
        make-backup-files nil
        auto-save-default nil
        ring-bell-function 'ignore
        confirm-kill-emacs 'y-or-n-p)
  
  ;; Window splitting preference
  (setq split-height-threshold nil
        split-width-threshold 140))

;; Theme and modeline
(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-height 23)
  (doom-modeline-minor-modes t))

(use-package minions
  :config
  (minions-mode))

;; Evil mode and related packages
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  
  ;; Key bindings
  (evil-define-key 'normal 'global
    (kbd ",w") 'save-buffer
    (kbd ",q") 'evil-delete-buffer
    (kbd "K") 'evil-previous-line
    (kbd ",c") 'evilnc-comment-or-uncomment-lines
    (kbd "gd") 'lsp-find-definition
    (kbd "gh") 'lsp-describe-thing-at-point)
  
  ;; More key bindings can be added here
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Completion framework
(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 14)
  (company-require-match 'never))

(use-package company-posframe
  :after company
  :config
  (company-posframe-mode))

;; LSP support
(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-keymap-prefix "s-i")
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 2000)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (remove-hook 'lsp-configure-hook 'lsp-headerline-breadcrumb-mode))

;; Clojure development
(use-package clojure-mode
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (require 'clojure-mode-extra-font-locking))

(use-package cider
  :after (clojure-mode)
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-result-prefix ";; => "))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (cider-repl-mode . lispy-mode))
  :custom
  (lispy-close-quotes-at-end-p t))

;; Navigation and search
(use-package ivy
  :config
  (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-re-builders-alist
   '((ivy-switch-buffer . ivy--regex-plus)
     (swiper . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  (ivy-initial-inputs-alist nil))

(use-package counsel
  :after ivy
  :config
  (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

;; Project management
(use-package projectile
  :config
  (projectile-global-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Git integration
(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; File tree
(use-package treemacs
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t 1" . treemacs-delete-other-windows)))

(provide 'newinit)
;;; newinit.el ends here
