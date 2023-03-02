(require 'flycheck)
(require 'prettier-js)
;; (require 'js2-refactor)
(require 'web-mode)

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

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

(add-hook 'web-mode-hook
          (lambda ()
            (add-node-modules-path)

            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?' . ?') (?` . ?`))))

            (electric-pair-mode)
            (electric-indent-mode)
            (emmet-mode)

            (evil-matchit-mode)

            (when (string-match "vue" (file-name-extension buffer-file-name))
              (flycheck-mode)
              (prettier-js-mode)
              (setq-local lsp-enabled-clients '(vue-semantic-server))
              ;; (setq-local lsp-file-watch-ignored-directories '())
              ;; (lsp)
              (flycheck-add-next-checker 'lsp 'javascript-eslint)
              )
          ))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (flycheck-mode)
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
            (setq-local lsp-disabled-clients '(vue-semantic-server))
            (lsp)
            (flycheck-add-next-checker 'lsp 'javascript-eslint)
            ;; (flycheck-add-next-checker 'lsp '(t . javascript-eslint))
            (setq-local company-backends '(company-capf))))

(add-hook 'css-mode-hook (lambda ()
                           (add-node-modules-path)
                           (prettier-js-mode)))

(add-hook 'scss-mode-hook (lambda ()
                            (add-node-modules-path)
                            (prettier-js-mode)))

;; FIXME: this is not working, why?!?
(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?$"
  :hook
  ((typescript-mode . (lambda ()
                        (setq typescript-indent-level 2)
                        (add-node-modules-path)
                        (lsp)
                        (tree-sitter-hl-mode)
                        ;; (my/ensure-curly-square-shortcut)
                        (prettier-js-mode)))))

(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)
;; (use-package prettier-js :ensure t :hook (typescript-mode))

;; couldn't make it work with `use-package`, plain elisp instead
;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)
;; (add-hook 'typescript-mode-hook (lambda ()
;;                                   (tree-sitter-hl-mode)
;;                                   (lsp)))
