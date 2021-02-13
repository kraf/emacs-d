;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
(require 'cider)

;; (require 'flycheck-clj-kondo)
;; (setq flycheck-clj-kondo-clj-executable (executable-find "clj-kondo"))
;; (setq flycheck-clj-kondo-cljs-executable (executable-find "clj-kondo"))
;; (setq flycheck-clj-kondo-edn-executable (executable-find "clj-kondo"))
;; (setq flycheck-clj-kondo-cljc-executable (executable-find "clj-kondo"))

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

(defun clojure-hook ()
  ;; (add-to-list 'flycheck-checkers 'flycheck-clj-kondo)
  ;; (setq inferior-lisp-program "lein repl")
  (font-lock-add-keywords
   nil
   '(("(\\(facts?\\)"
      (1 font-lock-keyword-face))
     ("(\\(background?\\)"
      (1 font-lock-keyword-face))))
  ;; (flycheck-mode)
  ;; (smartparens-mode)
  ;; (smartparens-strict-mode)
  ;; (evil-cleverparens-mode)
  (multiple-cursors-mode)
  (zprint-mode)
  (highlight-parentheses-mode)
  ;; (define-clojure-indent (fact 1))
  ;; (define-clojure-indent (facts 1))

  (lsp)
  (lispy-mode)
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  
  (define-key key-translation-map (kbd "ö") nil)
  (define-key key-translation-map (kbd "ä") nil)
  
  (evil-local-set-key 'normal ",eb" 'cider-eval-buffer)
  (evil-local-set-key 'normal ",ef" 'cider-eval-defun-at-point)
  (evil-local-set-key 'normal ",ee" 'cider-eval-last-sexp)
  (evil-local-set-key 'insert (kbd "M-n") 'lispy-forward)
  (evil-local-set-key 'insert (kbd "M-p") 'lispy-backward)
  (evil-local-set-key 'insert (kbd "ö") 'lispy-braces)
  (evil-local-set-key 'insert (kbd "ä") 'lispy-brackets)
  (evil-local-set-key 'insert (kbd "M-ä") 'lispy-wrap-braces)
  (evil-local-set-key 'insert (kbd "M-ö") 'lispy-wrap-brackets)
  
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t))



;; syntax hilighting for midje
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'clojurescript-mode-hook 'clojure-hook)

;; enable paredit in your REPL
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;;;;
;; Cider
;;;;

(use-package cider
  :commands cider
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-result-prefix ";; => ")
  
  ;; When there's a cider error, show its buffer and switch to it
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Wrap when navigating history.
  (cider-repl-wrap-history t)

  :config
  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (evil-insert-state)
                                    (cider-turn-on-eldoc-mode))))
