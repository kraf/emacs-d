(add-hook 'ruby-mode-hook
          (lambda ()
            (electric-pair-mode)
            (electric-indent-mode)
            (lsp)))
