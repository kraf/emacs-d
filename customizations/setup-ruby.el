;;;;
;; Ruby
;;;;

;; The language server is ruby-lsp (installed via mise/Gemfile);
;; solargraph (ruby-ls) is disabled in setup-lsp.el.

(defun my/ruby-mode-setup ()
  "Shared setup for Ruby buffers."
  (evil-matchit-mode 1)
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (lsp-deferred))

(add-hook 'ruby-mode-hook #'my/ruby-mode-setup)
(add-hook 'ruby-ts-mode-hook #'my/ruby-mode-setup)

;; Run specs from any buffer; uses `bundle exec rspec' when a Gemfile is
;; present, matching the project workflow.
(use-package rspec-mode
  :defer t
  :custom
  (rspec-use-bundler-when-possible t)
  :config
  (my-leader-def
    :states 'normal
    :keymaps '(rspec-mode-map rspec-verifiable-mode-map)
    "t" '(:ignore t :which-key "rspec")
    "tt" 'rspec-verify-single
    "tf" 'rspec-verify
    "tl" 'rspec-rerun
    "ts" 'rspec-toggle-spec-and-target))

(use-package inf-ruby
  :hook (ruby-base-mode . inf-ruby-minor-mode))
