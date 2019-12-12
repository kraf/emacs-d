;; javascript / html
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

(require 'flycheck)
(require 'prettier-js)

(setq flycheck-disabled-checkers '(javascript-jshint))
;; (add-to-list 'flycheck-checkers 'javascript-eslint)
;; (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
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
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-mode))
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?' . ?') (?` . ?`))))
            (electric-pair-mode)
            (electric-indent-mode)))

(add-hook 'rjsx-mode-hook (lambda ()
                            (flycheck-mode)
                            (electric-pair-mode)
                            (electric-indent-mode)
                            (js2-mode-hide-warnings-and-errors)
                            (prettier-js-mode)
                            (setq-local sgml-basic-offset 2)
                            (setq-local js2-basic-offset 2)
                            ))

(add-hook 'css-mode-hook (lambda ()
                           (prettier-js-mode)))

(add-hook 'scss-mode-hook (lambda ()
                            (prettier-js-mode)))

;; typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (prettier-js-mode)
  (electric-pair-mode)
  (electric-indent-mode)
  (company-mode +1)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-local typescript-indent-level 2)
  )

(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
