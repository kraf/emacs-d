(require 'cl-lib)
(require 'treesit)

;; JavaScript / TypeScript / web stack
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[mc]?js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))

;; Where grammars live; Emacs will look here for libtree-sitter-*.so.
;; Grammar sources and installs are handled by treesit-auto (the Vue
;; grammar is the exception, see setup-vue.el).
(add-to-list 'treesit-extra-load-path
             (expand-file-name "tree-sitter" user-emacs-directory))

(setq js-indent-level 2
      typescript-ts-mode-indent-offset 2
      tsx-ts-mode-indent-offset 2
      css-indent-offset 2
      flycheck-disabled-checkers '(javascript-jshint))

(use-package web-mode
  :mode "\\.html?\\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))

(use-package add-node-modules-path
  :defer t)

(use-package prettier-js
  :defer t)

(use-package npm-mode
  :defer t)

(defconst my/javascript-eslint-config-files
  '("eslint.config.js"
    "eslint.config.cjs"
    "eslint.config.mjs"
    "eslint.config.ts"
    ".eslintrc.js"
    ".eslintrc.cjs"
    ".eslintrc.yaml"
    ".eslintrc.yml"
    ".eslintrc.json"
    ".eslintrc")
  "ESLint config files that enable project-local linting.")

(defun my/javascript-project-uses-eslint-p ()
  (let ((project-root (or (buffer-file-name) default-directory)))
    (cl-some (lambda (file)
               (locate-dominating-file project-root file))
             my/javascript-eslint-config-files)))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/node-formatting-mode-setup ()
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (add-node-modules-path)
  (prettier-js-mode 1))

(defun my/node-lsp-mode-setup (&optional enable-npm-mode)
  (my/node-formatting-mode-setup)
  (when enable-npm-mode
    (npm-mode 1))
  (flycheck-mode 1)
  (when (my/javascript-project-uses-eslint-p)
    (my/use-eslint-from-node-modules))
  (setq-local company-backends '(company-capf)
              lsp-enable-on-type-formatting nil)
  (lsp-deferred))

(defun my/javascript-ts-mode-setup ()
  (evil-matchit-mode 1)
  (my/node-lsp-mode-setup t))

(defun my/json-ts-mode-setup ()
  (my/node-lsp-mode-setup))

(defun my/css-mode-setup ()
  (my/node-formatting-mode-setup))

(with-eval-after-load 'flycheck
  (dolist (mode '(web-mode js-ts-mode tsx-ts-mode typescript-ts-mode vue-ts-mode))
    (flycheck-add-mode 'javascript-eslint mode)))

(add-hook 'js-ts-mode-hook #'my/javascript-ts-mode-setup)
(add-hook 'tsx-ts-mode-hook #'my/javascript-ts-mode-setup)
(add-hook 'typescript-ts-mode-hook #'my/javascript-ts-mode-setup)
(add-hook 'json-ts-mode-hook #'my/json-ts-mode-setup)
(add-hook 'css-ts-mode-hook #'my/css-mode-setup)
(add-hook 'scss-mode-hook #'my/css-mode-setup)
(add-hook 'less-css-mode-hook #'my/css-mode-setup)
