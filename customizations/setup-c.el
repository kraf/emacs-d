(require 'company)

(add-hook 'c++-mode-hook (lambda ()
                           (electric-pair-mode)
                           (electric-indent-mode)
                           (flycheck-mode)
                           (add-to-list (make-local-variable 'company-backends)
                                        '(company-gtags
                                          company-keywords
                                          company-dabbrev))
                           (modify-syntax-entry ?_ "w")
                           ))

(add-to-list 'auto-mode-alist '("\\.cuh?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))
