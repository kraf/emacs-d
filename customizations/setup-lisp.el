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

  (lispyville--define-key 'normal ",c" 'lispyville-comment-or-uncomment-line)
  (lispyville--define-key 'visual ",c" 'lispyville-comment-or-uncomment)

  (lispyville--define-key 'insert (kbd "DEL") 'lispy-delete-backward)
  (lispyville--define-key 'insert (kbd "RET") 'lispy-newline-and-indent)
  (lispyville--define-key 'insert (kbd "\"") 'lispy-quotes)
  (lispyville--define-key 'insert (kbd "(") 'lispy-parens)
  (lispyville--define-key 'insert (kbd "{") 'lispy-braces)
  (lispyville--define-key 'insert (kbd "[") 'lispy-brackets)

  (lispyville--define-key 'normal (kbd "[") 'evil-forward-section-begin)
  (lispyville--define-key 'normal (kbd "]") 'evil-backward-section-begin)
  (lispyville--define-key '(insert normal) (kbd "M-ö") 'lispy-wrap-braces)
  (lispyville--define-key '(insert normal) (kbd "M-ä") 'lispy-wrap-brackets)
  (lispyville--define-key '(insert normal) (kbd "M-r") 'raise-sexp)

  (lispyville--define-key 'insert (kbd "C-y") 'lispy-yank)

  (lispyville--define-key 'normal "gd" 'lsp-find-definition)
  (lispyville--define-key 'normal (kbd "M-.") 'lsp-find-definition)

  (lispyville--define-key 'normal ",jc" 'lispy-clone)
  (lispyville--define-key 'normal (kbd "(") (lambda () (interactive) (avy-goto-char ?\()))
  (lispyville--define-key 'normal (kbd ")") (lambda () (interactive) (avy-goto-char ?\))))

  (lispyville--define-key 'normal "H" 'beginning-of-defun)
  (lispyville--define-key 'normal "L" 'end-of-defun))
