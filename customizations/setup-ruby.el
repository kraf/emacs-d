(add-hook 'ruby-mode-hook
          (lambda ()
            (evil-matchit-mode)
            (electric-pair-mode)
            (electric-indent-mode)
            ;; (lsp)
            ))
