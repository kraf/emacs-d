;; LSP core configuration, shared by all languages.

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "s-i")
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-file-watch-threshold 2000)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (remove-hook 'lsp-configure-hook 'lsp-headerline-breadcrumb-mode))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package flycheck
  :defer t)
