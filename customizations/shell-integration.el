;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; asdf
(add-to-list 'load-path "~/.emacs.d/vendor")

(require 'asdf)

(asdf-enable)
