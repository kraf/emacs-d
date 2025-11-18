(require 'flycheck)
(require 'prettier-js)
(require 'web-mode)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.s?css$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))

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

(require 'treesit)

;; Where grammars live; Emacs will look here for libtree-sitter-*.so
(add-to-list 'treesit-extra-load-path
             (expand-file-name "tree-sitter" user-emacs-directory))

(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((bash       "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
                (vue        "https://github.com/ikatyang/tree-sitter-vue")
                (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                (css        "https://github.com/tree-sitter/tree-sitter-css")
                (html       "https://github.com/tree-sitter/tree-sitter-html"))))

;; --- LSP + Flycheck integration (make 'lsp' a real Flycheck checker) ---
(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flycheck))  ;; ensures 'lsp' checker exists

(with-eval-after-load 'flycheck
  ;; Make sure ESLint runs in .vue and web-mode too
  (flycheck-add-mode 'javascript-eslint 'vue-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; --- Volar relies on a separate TS server (ts-ls) ---
(setq lsp-volar-typescript-server-id 'ts-ls)  ;; requires typescript-language-server installed

;; --- Vue SFCs with tree-sitter mode ---
(defun my/vue-ts-mode-setup ()
  (electric-pair-local-mode 1)
  (electric-indent-mode 1)
  (emmet-mode 1)
  (evil-matchit-mode 1)
            (add-node-modules-path)
  (lsp)
  (prettier-js-mode 1)
  (setq-local lsp-enable-on-type-formatting nil)
  (when (boundp 'lsp-enable-indentation)
    (setq-local lsp-enable-indentation nil))

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
      (flycheck-add-next-checker 'lsp 'javascript-eslint))))

;; Let lsp-mode recognize vue-ts-mode as "vue"
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(vue-ts-mode . "vue")))

(use-package vue-ts-mode
  :straight (vue-ts-mode :type git :host github :repo "8uff3r/vue-ts-mode")
  :mode "\\.vue\\'"
  :init
  (setq vue-ts-mode-indent-offset 2) ;; 2 spaces; tweak to taste
  :hook
  ;; Recreate your old vue-mode-hook behavior here:
  ((vue-ts-mode . my/vue-ts-mode-setup)))

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
