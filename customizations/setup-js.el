(require 'flycheck)
(require 'prettier-js)
;; (require 'js2-refactor)
(require 'web-mode)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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

(add-hook 'web-mode-hook
          (lambda ()
            (flycheck-mode)
            (electric-pair-mode)
            (electric-indent-mode)
            (npm-mode)

            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?' . ?') (?` . ?`))))
            
            (when (string-match "tsx?" (file-name-extension buffer-file-name))
              (prettier-js-mode)
              (lsp)
              (flycheck-add-next-checker 'lsp 'javascript-eslint)
              ;; FIXME temporary until lsp-eslint works again
              ;; (flymake-mode)
              ;; (flymake-eslint-enable)
              (setq-local company-backends '(company-capf)))))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (flycheck-mode)
            (electric-pair-mode)
            (electric-indent-mode)
            (npm-mode)
            
            (setq-local sgml-basic-offset 2)
            (setq-local js2-basic-offset 2)

            (prettier-js-mode)
            (lsp)
            ;; FIXME temporary until lsp-eslint works again
            ;; (flymake-mode)
            ;; (flymake-eslint-enable)
            (flycheck-add-next-checker 'lsp '(t . javascript-eslint))
            (setq-local company-backends '(company-capf))))

(add-hook 'css-mode-hook (lambda ()
                           (prettier-js-mode)))

(add-hook 'scss-mode-hook (lambda ()
                            (prettier-js-mode)))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?$"
  :custom
  (lsp))

(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)
(use-package prettier-js :ensure t :hook (typescript-mode))

;; couldn't make it work with `use-package`, plain elisp instead
(require 'tree-sitter)
(require 'tree-sitter-langs)
(add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)
