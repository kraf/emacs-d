(require 'magit)
;; (setq magit-last-seen-setup-instructions "1.4.0")
;; (setq git-commit-summary-max-length 79)

;; (require 'evil-magit)

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(with-eval-after-load 'magit
  (require 'forge)
  (define-key evil-visual-state-map "s" 'magit-stage))
