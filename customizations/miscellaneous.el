;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts (setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(setq ag-highlight-search t)

(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

(setq confirm-kill-emacs 'y-or-n-p)
