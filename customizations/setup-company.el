;; Keep global completion defaults in one place. Language-specific modules can
;; still set mode-local backends when needed.

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package company
  :hook (after-init . global-company-mode)
  :bind ("C-." . company-complete)
  :custom
  (company-backends '(company-capf))
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 14)
  (company-echo-delay nil)
  (company-minimum-prefix-length 2)
  (company-idle-delay 0)
  (company-require-match 'never)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)))

(use-package company-posframe
  :after company
  :config
  (company-posframe-mode))
