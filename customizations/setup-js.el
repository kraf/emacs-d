(require 'flycheck)
(require 'prettier-js)
;; (require 'js2-refactor)
(require 'web-mode)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (js2r-add-keybindings-with-prefix "C-c C-m")

(setq flycheck-disabled-checkers '(javascript-jshint))
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

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

(add-hook 'web-mode-hook
          (lambda ()
            (flycheck-mode)
            (electric-pair-mode)
            (electric-indent-mode)
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?' . ?') (?` . ?`))))
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-css-indent-offset 2)

            (prettier-js-mode)
            (lsp)))

(add-hook 'css-mode-hook (lambda ()
                           (prettier-js-mode)))

(add-hook 'scss-mode-hook (lambda ()
                            (prettier-js-mode)))

