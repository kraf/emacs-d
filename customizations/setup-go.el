(require 'flycheck)

(add-to-list 'flycheck-checkers 'go-build)
(flycheck-add-mode 'go-build 'go-mode)

(add-hook 'go-mode-hook
          (lambda ()
            (electric-pair-mode)
            (electric-indent-mode)
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))

