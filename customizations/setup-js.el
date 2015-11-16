;; javascript / html
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(require 'flycheck)

(setq flycheck-disabled-checkers '(javascript-jshint))
(setq flycheck-checkers '(javascript-eslint))
(flycheck-add-mode 'javascript-eslint 'web-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-mode))
            (electric-pair-mode)
            (electric-indent-mode)))

(add-hook 'js2-mode-hook (lambda ()
                           (flycheck-mode)
                           (electric-pair-mode)
                           (electric-indent-mode)
                           (js2-mode-hide-warnings-and-errors)))

;; typescript
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode)
            (electric-pair-mode)
            (electric-indent-mode)))

(setq company-tooltip-align-annotations t)

(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode))))

(define-key evil-normal-state-map ",td" 'tide-jump-to-definition)
(define-key evil-normal-state-map ",tt" 'tide-jump-back)
(define-key evil-normal-state-map ",tr" 'tide-rename-symbol)
(define-key evil-normal-state-map ",tl" 'tide-references)
