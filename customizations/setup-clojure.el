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

(with-eval-after-load 'lispyville
  (lispyville--define-key 'normal ",ll" #'lsp-clojure-add-missing-libspec)
  (lispyville--define-key 'normal ",eb" #'cider-eval-buffer)
  (lispyville--define-key 'normal ",ef" #'cider-eval-defun-at-point)
  (lispyville--define-key 'normal ",ee" #'cider-eval-sexp-at-point)
  (lispyville--define-key 'normal ",en" #'cider-eval-ns-form)
  (lispyville--define-key 'normal ",el" #'cider-eval-list-at-point)
  (lispyville--define-key 'visual ",ee" #'cider-insert-region-in-repl)
  (lispyville--define-key 'normal ",jr" #'my/cider-user-fast-reset)
  (lispyville--define-key 'normal ",jR" #'my/cider-user-reset)
  (lispyville--define-key 'normal ",jf" #'my/cider-refresh)
  (lispyville--define-key 'normal ",jt" #'my/cider-reveal-tap-log))

(add-to-list 'auto-mode-alist '("lein-env" . ruby-mode))

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
