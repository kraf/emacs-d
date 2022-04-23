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
  ;; (zprint-mode)
  (highlight-parentheses-mode)
  ;; (define-clojure-indent (fact 1))
  ;; (define-clojure-indent (facts 1))
  ;; (clj-refactor-mode)

  (lispy-mode)
  (lispyville-mode)
  
  (lsp)
  ;; Fix clojure-lsp conflicts with other modes
  (setq cljr-add-ns-to-blank-clj-files nil)
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq lsp-keep-workspace-alive nil)
  
  ;; (setq completion-at-point-functions '(cider-complete-at-point))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  
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

  (local-set-key (kbd "C-c C-e") 'cider-eval-sexp-at-point)

  :config
  (add-hook 'cider-repl-mode-hook 'evil-insert-state))
