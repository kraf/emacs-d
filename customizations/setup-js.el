(require 'flycheck)
(require 'prettier-js)
;; (require 'js2-refactor)
(require 'web-mode)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; (js2r-add-keybindings-with-prefix "C-c C-m")

(setq flycheck-disabled-checkers '(javascript-jshint))
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; (setq lsp-eslint-server-command
;;       '("node"
;;         ;; "/home/filip/src/github/vscode-eslint/server/out/eslintServer.js"
;;         "/home/filip/.vscode/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js"
;;         "--stdio"))

;; (defun my/ensure-curly-square-shortcut ()
;;   (define-key key-translation-map (kbd "ö") (kbd "{"))
;;   (define-key key-translation-map (kbd "ä") (kbd "["))
;;   )

(require 'treesit)

;; Where grammars live; Emacs will look here for libtree-sitter-*.so
(add-to-list 'treesit-extra-load-path
             (expand-file-name "tree-sitter" user-emacs-directory))

(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((vue        "https://github.com/ikatyang/tree-sitter-vue")
                (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                (css        "https://github.com/tree-sitter/tree-sitter-css")
                (html       "https://github.com/tree-sitter/tree-sitter-html"))))

;; Register Vue language server (Volar) with LSP mode
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("vue-language-server" "--stdio"))
                    :major-modes '(vue-ts-mode)
                    :server-id 'volar
                    :priority 1
                    :initialization-options (lambda ()
                                              (list :typescript (list :tsdk (concat (lsp-workspace-root) "/node_modules/typescript/lib"))))
                    :notification-handlers (ht ("$/typescriptVersion" #'ignore)))))

(use-package vue-ts-mode
  :straight (vue-ts-mode :type git :host github :repo "8uff3r/vue-ts-mode")
  :mode "\\.vue\\'"
  :init
  (setq vue-ts-mode-indent-offset 2) ;; 2 spaces; tweak to taste
  :hook
  ;; Recreate your old vue-mode-hook behavior here:
  ((vue-ts-mode . my/vue-ts-mode-setup)
   (vue-ts-mode . lsp-deferred)
   (vue-ts-mode . add-node-modules-path))
  :config
  ;; Ensure Tree-sitter grammars can be installed/found
  (require 'treesit)
  (setq treesit-language-source-alist
        (append treesit-language-source-alist
                '((vue        "https://github.com/ikatyang/tree-sitter-vue")
                  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                  (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                  (css        "https://github.com/tree-sitter/tree-sitter-css")
                  ;; optional, but nice to have in SFC templates:
                  (html       "https://github.com/tree-sitter/tree-sitter-html"))))
  ;; (Optional but harmless) Make sure Emacs searches this default install dir:
  (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" user-emacs-directory)))

;; Let lsp-mode recognize vue-ts-mode buffers as "vue"
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(vue-ts-mode . "vue")))

;; Your per-buffer setup, mirroring the old vue-mode hook
(defun my/vue-ts-mode-setup ()
  "Local tweaks for Vue SFCs using vue-ts-mode."
  ;; What you had before:
  (electric-pair-local-mode 1)
  (electric-indent-mode 1)
  (emmet-mode 1)
  (evil-matchit-mode 1)
  (flycheck-mode 1)
  (prettier-js-mode 1)

  ;; Prefer Prettier for formatting; avoid on-type LSP formatting jitter.
  (setq-local lsp-enable-on-type-formatting nil)
  (when (boundp 'lsp-enable-indentation)
    (setq-local lsp-enable-indentation nil))

  ;; Make sure eslint runs in .vue buffers (after LSP diagnostics).
  ;; (This mirrors your previous: flycheck-add-next-checker 'lsp 'javascript-eslint)
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'vue-ts-mode)
    (flycheck-add-next-checker 'lsp 'javascript-eslint)))

;; ;; Register Vue language server (Volar) with LSP mode
;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("vue-language-server" "--stdio"))
;;                     :major-modes '(vue-mode)
;;                     :server-id 'volar
;;                     :priority 1
;;                     :initialization-options (lambda ()
;;                                               (list :typescript (list :tsdk (concat (lsp-workspace-root) "/node_modules/typescript/lib"))))
;;                     :notification-handlers (ht ("$/typescriptVersion" #'ignore)))))

;; (add-hook 'vue-mode-hook
;;           (lambda ()
;;             (add-node-modules-path)

;;             ;; Basic modes without problematic electric-pair customization
;;             (electric-pair-local-mode 1)
;;             (electric-indent-mode)
;;             (emmet-mode)
;;             (evil-matchit-mode)
;;             (flycheck-mode)
;;             (prettier-js-mode)

;;             ;; Start LSP for Vue files
;;             (lsp)
;;             (flycheck-add-next-checker 'lsp 'javascript-eslint)
;;             ))
;; (defun my-rjsx-mode-eslint-setup ()
;;   "Disable Flycheck in rjsx-mode if eslint executable or config is missing."
;;   )

(use-package vue-ts-mode
  :straight (vue-ts-mode :type git :host github :repo "8uff3r/vue-ts-mode")
  :mode "\\.vue\\'"
  :init
  (setq vue-ts-mode-indent-offset 2)  ;; tweak if you like 2 vs 4
  :hook ((vue-ts-mode . lsp-deferred)
         (vue-ts-mode . add-node-modules-path)))

;; Let lsp-mode recognize vue-ts-mode as "vue"
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(vue-ts-mode . "vue")))



(add-hook 'rjsx-mode-hook
          (lambda ()
            (electric-pair-mode)
            (electric-indent-mode)
            (npm-mode)
            (evil-matchit-mode)

            (setq-local sgml-basic-offset 2)
            (setq-local js2-basic-offset 2)
            (setq-local js2-strict-missing-semi-warning nil)
            (setq-local js2-strict-inconsistent-return-warning nil)

            ;; (my/ensure-curly-square-shortcut)

            (add-node-modules-path)
            (prettier-js-mode)
            (lsp)

            (let ((eslint-configs '("eslint.config.js"
                                    ".eslintrc.js"
                                    ".eslintrc.cjs"
                                    ".eslintrc.yaml"
                                    ".eslintrc.yml"
                                    ".eslintrc.json"
                                    ".eslintrc")))
              (if (and (executable-find "eslint")
                       (some (lambda (file) (locate-dominating-file default-directory file))
                             eslint-configs))
                  (flycheck-mode)
                (flycheck-add-next-checker 'lsp 'javascript-eslint)))


            (setq-local company-backends '(company-capf))))

(add-hook 'css-mode-hook (lambda ()
                           (add-node-modules-path)
                           (prettier-js-mode)))

(add-hook 'scss-mode-hook (lambda ()
                            (add-node-modules-path)
                            (prettier-js-mode)))

;; (use-package prettier-js :ensure t :hook (typescript-mode))

;; couldn't make it work with `use-package`, plain elisp instead
;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)
;; (add-hook 'typescript-mode-hook (lambda ()
;;                                   (tree-sitter-hl-mode)
;;                                   (lsp)))
