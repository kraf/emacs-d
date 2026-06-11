;; Git tooling.

(use-package magit
  ;; Loaded eagerly: the after-save hook below must work from the first save.
  :demand t
  :config
  (add-hook 'after-save-hook #'magit-after-save-refresh-status t))

;; Stage region from visual state
(general-define-key
 :states 'visual
 "s" 'magit-stage)

(use-package git-timemachine
  :defer t
  :config
  ;; Let git-timemachine's keys win over evil's normal state.
  ;; @see https://bitbucket.org/lyro/evil/issue/511
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(use-package git-link
  :defer t)

(use-package browse-at-remote
  :defer t)
