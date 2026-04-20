;; Changes all yes/no questions to y/n type
(setq use-short-answers t)

;; shell scripts (setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(use-package ag
  :custom
  (ag-highlight-search t))

(use-package vkill
  :commands (vkill list-unix-processes))

(setq confirm-kill-emacs 'y-or-n-p)

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;; (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

;; (tramp-enable-method "distrobox")
