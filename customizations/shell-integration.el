;; Make Emacs inherit environment variables from the login shell.
;; https://github.com/purcell/exec-path-from-shell
;; Note: `window-system' is `pgtk' on Wayland, so don't gate on '(mac ns x).
(use-package exec-path-from-shell
  :config
  (when (or (daemonp) (display-graphic-p))
    (exec-path-from-shell-initialize)))
