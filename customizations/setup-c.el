(add-hook 'c++-mode-hook (lambda ()
                           ;; (flycheck-mode)
                           (electric-pair-mode)
                           (electric-indent-mode)))

(define-key evil-normal-state-map ",gd" 'xref-find-definitions)
(define-key evil-normal-state-map ",gr" 'xref-find-references)
