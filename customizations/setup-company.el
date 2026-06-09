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
  ;; Keep this above 0: with 0, company runs `completion-at-point-functions'
  ;; on every keystroke, turning any passive/blocking capf (CIDER nREPL,
  ;; git-commit's dabbrev-capf, ...) into a per-keystroke stall.
  (company-idle-delay 0.2)
  (company-require-match 'never)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)))

(use-package company-posframe
  :after company
  :config
  (company-posframe-mode))
