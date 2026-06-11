(add-hook 'ruby-mode-hook
          (lambda ()
            (evil-matchit-mode)
            (electric-pair-local-mode)
            (electric-indent-local-mode)))
