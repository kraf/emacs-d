;; Structural editing for lisps: lispy in lisp buffers, lispyville for evil
;; integration. Shared by elisp and Clojure.

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode lisp-interaction-mode clojure-mode) . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :defer t)

;; Work around a lispy bug: when `lispy-mode' starts in a `clojure-mode'
;; buffer, lispy.el runs `(setq completion-at-point-functions ...)' with a
;; plain `setq' instead of `setq-local', clobbering the *global* default.
;; Every other buffer (Ruby, magit commit messages, ...) then inherits
;; CIDER/lispy completion and runs it on each keystroke.  Make the variable
;; buffer-local before lispy-mode runs (depth -100 => first on the hook) so
;; lispy's `setq' only touches our local value and the global default stays
;; clean.
(add-hook 'clojure-mode-hook
          (lambda () (make-local-variable 'completion-at-point-functions))
          -100)

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (cider-repl-mode . lispy-mode))
  :custom
  (lispy-close-quotes-at-end-p t)
  :config
  (lispy-set-key-theme '())
  (lispyville-set-key-theme)

  (define-key lispy-mode-map-lispy "[" nil)
  (define-key lispy-mode-map-lispy "]" nil)
  (define-key lispy-mode-map-lispy "{" nil)
  (define-key lispy-mode-map-lispy "}" nil))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :custom
  (lispyville-key-theme '(operators
                          c-w
                          (prettify insert)
                          additional
                          additional-insert
                          additional-movement
                          additional-wrap
                          (atom-movement normal visual)
                          slurp/barf-cp))
  :config
  (define-key key-translation-map (kbd "ö") nil)
  (define-key key-translation-map (kbd "ä") nil)

  (general-define-key
   :states 'normal
   :keymaps 'lispyville-mode-map
   ",c" 'lispyville-comment-or-uncomment-line
   ",jc" 'lispy-clone
   "[" 'evil-forward-section-begin
   "]" 'evil-backward-section-begin
   "H" 'beginning-of-defun
   "L" 'end-of-defun
   "gd" 'lsp-find-definition
   "M-." 'lsp-find-definition
   "(" (lambda () (interactive) (avy-goto-char ?\())
   ")" (lambda () (interactive) (avy-goto-char ?\))))

  (general-define-key
   :states 'visual
   :keymaps 'lispyville-mode-map
   ",c" 'lispyville-comment-or-uncomment)

  (general-define-key
   :states 'insert
   :keymaps 'lispyville-mode-map
   "DEL" 'lispy-delete-backward
   "RET" 'lispy-newline-and-indent
   "\"" 'lispy-quotes
   "(" 'lispy-parens
   "{" 'lispy-braces
   "[" 'lispy-brackets
   "C-y" 'lispy-yank)

  (general-define-key
   :states '(insert normal)
   :keymaps 'lispyville-mode-map
   "M-ö" 'lispy-wrap-braces
   "M-ä" 'lispy-wrap-brackets
   "M-r" 'raise-sexp))
