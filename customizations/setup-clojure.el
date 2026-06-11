;;;;
;; Clojure
;;;;

(use-package zprint
  :straight nil
  :commands zprint-mode)

(defconst my/clojure-midje-font-lock-keywords
  '(("(\\(facts?\\)"
     (1 font-lock-keyword-face))
    ("(\\(background?\\)"
     (1 font-lock-keyword-face)))
  "Extra font-lock keywords for Midje forms.")

(defconst my/clojure-lsp-language-id-modes
  '(clojure-mode
    clojurec-mode
    clojurescript-mode
    clojurex-mode)
  "Clojure-derived modes that should be handled by clojure-lsp.")

(defun my/clojure-add-midje-font-lock ()
  (font-lock-add-keywords nil my/clojure-midje-font-lock-keywords))

(defun my/cider-user-reset ()
  "Full reset: halt the system, refresh all namespaces, restart."
  (interactive)
  (cider-interactive-eval "(user/reset)"))

(defun my/cider-user-fast-reset ()
  "Reload only changed namespaces in place (no system restart) and ensure
the dev system is running."
  (interactive)
  (cider-interactive-eval "(user/fast-reset)"))

(defun my/cider-refresh ()
  (interactive)
  (cider-interactive-eval "(clojure.tools.namespace.repl/refresh)"))

(defun my/cider-reveal-tap-log ()
  (interactive)
  (cider-interactive-eval
   "(require 'vlaaad.reveal)(vlaaad.reveal/tap-log)"))

(defun my/clojure-mode-setup ()
  "Shared setup for Clojure buffers."
  (my/clojure-add-midje-font-lock)
  (zprint-mode 1)
  (highlight-parentheses-mode 1)
  (prettify-symbols-mode 1)
  (setq-local evil-symbol-word-search t
              lsp-idle-delay 0.2
              lsp-keep-workspace-alive nil
              clojure-indent-style 'align-arguments
              clojure-align-forms-automatically t)
  (lsp-deferred))

(my-leader-def
  :states 'normal
  :keymaps 'lispyville-mode-map
  "ll" #'lsp-clojure-add-missing-libspec

  "e" '(:ignore t :which-key "eval")
  "eb" #'cider-eval-buffer
  "ef" #'cider-eval-defun-at-point
  "ee" #'cider-eval-sexp-at-point
  "en" #'cider-eval-ns-form
  "el" #'cider-eval-list-at-point

  "j" '(:ignore t :which-key "repl")
  "jr" #'my/cider-user-fast-reset
  "jR" #'my/cider-user-reset
  "jf" #'my/cider-refresh
  "jt" #'my/cider-reveal-tap-log)

(my-leader-def
  :states 'visual
  :keymaps 'lispyville-mode-map
  "ee" #'cider-insert-region-in-repl)

(add-to-list 'auto-mode-alist '("lein-env" . ruby-mode))

(use-package clojure-mode
  :mode (("\\.edn\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode))
  :hook ((clojure-mode . my/clojure-mode-setup)
         (clojurescript-mode . my/clojure-mode-setup)
         (clojurec-mode . my/clojure-mode-setup)
         (clojurex-mode . my/clojure-mode-setup))
  :config
  (define-clojure-indent
    (match 1))
  (with-eval-after-load 'lsp-mode
    (dolist (mode my/clojure-lsp-language-id-modes)
      (add-to-list 'lsp-language-id-configuration `(,mode . "clojure")))))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package flycheck-clj-kondo
  :after flycheck)

(use-package cider
  :commands cider
  :bind (:map cider-mode-map
              ("C-c C-e" . cider-eval-sexp-at-point))
  :hook ((cider-repl-mode . evil-insert-state)
         (cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-result-prefix ";; => ")
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-wrap-history t)
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-format-code-options
   '(("indents" (("plait" (("inner" 0)))
                 ("match" (("inner" 0))))))))
