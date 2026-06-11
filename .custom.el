(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function 'split-window-horizontally)
 '(flycheck-locate-config-file-functions '(flycheck-locate-config-file-ancestor-directories))
 '(magit-pull-arguments '("--rebase"))
 '(org-startup-truncated nil)
 '(safe-local-variable-directories
   '("/home/filip/src/gomore/backend/web/"
     "/home/filip/src/gomore/backend/api/"
     "/home/filip/src/gomore/backend-beta/api/"
     "/home/filip/src/gomore/backend-beta/"
     "/home/filip/src/gomore/backend/"
     "/home/filip/src/gomore/backend-alpha/"))
 '(select-enable-primary nil)
 '(tramp-completion-reread-directory-timeout 0)
 '(tramp-default-method "scp")
 '(tramp-remote-path
   '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin"
                               "/usr/local/bin" "/usr/local/sbin"
                               "/local/bin" "/local/freeware/bin"
                               "/local/gnu/bin" "/usr/freeware/bin"
                               "/usr/pkg/bin" "/usr/contrib/bin"
                               "/opt/bin" "/opt/sbin" "/opt/local/bin"
                               tramp-own-remote-path))
 '(tramp-verbose 2)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-deprecated-face ((t (:strike-through t)))))
