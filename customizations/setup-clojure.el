;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)

(require 'flycheck-clj-kondo)
(setq flycheck-clj-kondo-clj-executable (executable-find "clj-kondo"))
(setq flycheck-clj-kondo-cljs-executable (executable-find "clj-kondo"))
(setq flycheck-clj-kondo-edn-executable (executable-find "clj-kondo"))
(setq flycheck-clj-kondo-cljc-executable (executable-find "clj-kondo"))

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (add-to-list 'flycheck-checkers 'flycheck-clj-kondo)
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (flycheck-mode)
            (smartparens-mode)
            (smartparens-strict-mode)
            (evil-cleverparens-mode)
            (zprint-mode)
            (highlight-parentheses-mode)
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

(defun add-clj-format-before-save ()
  (interactive)
  (add-hook 'before-save-hook
            'cider-format-buffer
            t
	    t))

(add-hook 'clojure-mode-hook 'add-clj-format-before-save)

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(require 'evil)
(evil-define-key 'normal clojure-mode-map (kbd "M-ä") 'evil-cp-wrap-next-square)
(evil-define-key 'normal clojure-mode-map (kbd "M-ö") 'evil-cp-wrap-next-curly)
