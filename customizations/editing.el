;; General editing behavior.

;; Prefer tree-sitter major modes and install grammars on demand.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

;; Where grammars live; Emacs will look here for libtree-sitter-*.so.
;; Grammar sources/installs are handled by treesit-auto (the Vue grammar is
;; the exception, see setup-vue.el).
(require 'treesit)
(add-to-list 'treesit-extra-load-path
             (expand-file-name "tree-sitter" user-emacs-directory))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; Reopen files at the last visited position.
(use-package saveplace
  :straight nil
  :custom
  (save-place-file (concat user-emacs-directory "places"))
  :config
  (save-place-mode 1))

;; Keep backups and auto-saves out of the working directories.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(let ((auto-save-dir (expand-file-name "auto-save-list/" user-emacs-directory)))
  (make-directory auto-save-dir t)
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir)))
(setq auto-save-default t)
(setq require-final-newline t)

(use-package multiple-cursors
  :config
  (setq mc/always-repeat-command 1)
  (setq mc/always-run-for-all 1))

;; Visual undo tree on top of the built-in undo-redo system.
(use-package vundo
  :commands vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(my-leader-def
  :states 'normal
  "u" 'vundo)

(defun my/enable-delete-trailing-whitespace-on-save ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'my/enable-delete-trailing-whitespace-on-save)

;;; Global key bindings (built-in commands not owned by a package)

;; Lisp-friendly hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Don't suspend/minimize the frame.
(global-unset-key (kbd "C-x z"))
(global-unset-key (kbd "C-x C-z"))
