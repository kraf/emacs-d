(require 'company)

(add-hook 'c++-mode-hook (lambda ()
                           (electric-pair-mode)
                           (electric-indent-mode)
                           (ggtags-mode)
                           (add-to-list (make-local-variable 'company-backends)
                                        '(company-gtags company-keywords))
                           (modify-syntax-entry ?_ "w")
                           ))

(define-key evil-normal-state-map ",g" 'ggtags-find-tag-dwim)

(add-to-list 'auto-mode-alist '("\\.cuh?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))
