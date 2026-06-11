;;; init.el --- Filip's Emacs configuration -*- lexical-binding: t; -*-

;; Package management: straight.el + use-package. Every package is declared
;; with a `use-package' block in the customizations/ file that configures it.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'use-package)

;; Unmanaged local elisp (zprint).
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Machine-local settings that must run before everything else (gitignored).
(load "local-before.el")

;; Environment
(load "shell-integration.el")

;; Core editing and UI
(load "evil-mode.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "miscellaneous.el")

;; Lisp editing
(load "setup-lisp.el")

;; LSP core shared by all languages
(load "setup-lsp.el")

;; Languages
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-vue.el")
(load "setup-ruby.el")

(load "orgmode.el")
(load "magit-custom.el")
(load "setup-company.el")
(load "terminal.el")

;; Machine-local settings (gitignored)
(load "local.el")

;; Keep Customize state out of init.el.
(setq custom-file (concat user-emacs-directory ".custom.el"))
(ignore-errors
  (load-file custom-file))

(require 'server)
(unless (server-running-p) (server-start))
