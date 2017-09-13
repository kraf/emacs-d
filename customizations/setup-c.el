(add-hook 'c++-mode-hook (lambda ()
                           ;; (flycheck-mode)
                           (electric-pair-mode)
                           (electric-indent-mode)))
