;;;;
;; Ruby
;;;;

;; Navigation/docs/completion come from robe-mode, which introspects a live
;; Ruby console instead of static analysis. Linting is flycheck's built-in
;; `ruby-rubocop' checker.

(defun my/ruby-mode-setup ()
  "Shared setup for Ruby buffers."
  (evil-matchit-mode 1)
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (flycheck-mode 1))

(add-hook 'ruby-mode-hook #'my/ruby-mode-setup)
(add-hook 'ruby-ts-mode-hook #'my/ruby-mode-setup)

(use-package robe
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode))
  :config
  (general-define-key
   :states 'normal
   :keymaps 'robe-mode-map
   "gd" 'robe-jump
   "M-." 'robe-jump)
  (my-leader-def
    :states 'normal
    :keymaps 'robe-mode-map
    "lr" 'xref-find-references))

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
