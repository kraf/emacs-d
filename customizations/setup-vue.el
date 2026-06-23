(require 'treesit)
(require 'lsp-volar)

(use-package emmet-mode
  :defer t)

(add-to-list 'treesit-language-source-alist
             '(vue "https://github.com/ikatyang/tree-sitter-vue"))

(setq lsp-volar-typescript-server-id 'ts-ls)

(defun my/install-vue-treesit-grammar ()
  "Install the Vue tree-sitter grammar under `user-emacs-directory'."
  (interactive)
  (let ((grammar-dir (expand-file-name "tree-sitter" user-emacs-directory)))
    (make-directory grammar-dir t)
    (treesit-install-language-grammar 'vue grammar-dir)))

(defun my/vue-mode-setup ()
  "Shared setup for Vue buffers."
  (my/node-formatting-mode-setup)
  (evil-matchit-mode 1)
  (emmet-mode 1)
  (flycheck-mode 1)
  (when (my/javascript-project-uses-eslint-p)
    (my/use-eslint-from-node-modules))
  (setq-local lsp-enable-on-type-formatting nil)
  (lsp-deferred))

(defun my/vue-web-mode-hook ()
  "Setup Vue buffers that fall back to `web-mode'."
  (when (and buffer-file-name
             (string-suffix-p ".vue" buffer-file-name))
    (my/vue-mode-setup)))

(defun my/vue-grammar-ready-p ()
  "Return non-nil when the Vue tree-sitter grammar is available."
  (and (fboundp 'treesit-ready-p)
       (treesit-ready-p 'vue t)))

(defun my/vue-major-mode ()
  "Prefer `vue-ts-mode' for Vue buffers, falling back to `web-mode'."
  (if (my/vue-grammar-ready-p)
      (progn
        (require 'vue-ts-mode)
        (vue-ts-mode))
    (message "Vue tree-sitter grammar missing; run M-x my/install-vue-treesit-grammar")
    (web-mode)))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . my/vue-major-mode))

(use-package vue-ts-mode
  :straight (vue-ts-mode :type git
                         :host github
                         :repo "8uff3r/vue-ts-mode")
  :defer t
  :init
  (setq vue-ts-mode-indent-offset 2)
  :hook
  (vue-ts-mode . my/vue-mode-setup))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(vue-ts-mode . "vue")))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'vue-ts-mode))

(add-hook 'web-mode-hook #'my/vue-web-mode-hook)
