;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
(setq require-final-newline t)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)
(setq require-final-newline t)

(defun toggle-filmode ()
  (interactive)
  (if (get 'toggle-filmode 'state)
      (progn
        (define-key key-translation-map (kbd "ö") (kbd "ö"))
        (define-key key-translation-map (kbd "ä") (kbd "ä"))
        (put 'toggle-filmode 'state nil))
    (progn
      (define-key key-translation-map (kbd "ö") (kbd "{"))
      (define-key key-translation-map (kbd "ä") (kbd "["))
      (put 'toggle-filmode 'state t))))

(toggle-filmode)

(require 'paredit)
(add-hook 'paredit-mode-hook
          (lambda () 
            (local-set-key (kbd "C-ä") 'paredit-forward-slurp-sexp)
            (local-set-key (kbd "C-Ä") 'paredit-forward-barf-sexp)
            (local-set-key (kbd "C-ö") 'paredit-backward-slurp-sexp)
            (local-set-key (kbd "C-Ö") 'paredit-backward-barf-sexp)))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map (kbd "C-.") 'company-complete)

; (setq mac-option-modifier 'meta)
;; (setq mac-command-modifier 'meta)
(setq mac-right-option-modifier nil)
(setq mac-right-command-modifier nil)

;; (global-set-key (kbd "A-7") (kbd "{"))
;; (global-set-key (kbd "A-8") (kbd "["))
;; (global-set-key (kbd "A-9") (kbd "]"))
;; (global-set-key (kbd "A-0") (kbd "}"))
;; (global-set-key (kbd "A-<") (kbd "|"))
;; (global-set-key (kbd "A-+") (kbd "~"))
;; (global-set-key (kbd "A-ß") (kbd "\\"))
;; (global-set-key (kbd "A-q") (kbd "@"))

(require 'yasnippet)
(yas-global-mode 1)

(require 'multiple-cursors)
(setq mc/always-repeat-command 1)
(setq mc/always-run-for-all 1)
