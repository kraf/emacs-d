;; Hard-to-categorize customizations.

;; Changes all yes/no questions to y/n type
(setq use-short-answers t)

;; No need for lockfiles (.#foo) when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(setq confirm-kill-emacs 'y-or-n-p)

(use-package vkill
  :commands (vkill list-unix-processes))

;; Editor Code Assistant (AI pair programming)
(use-package eca
  :straight (eca :type git
                 :host github
                 :repo "editor-code-assistant/eca-emacs")
  :commands (eca))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))
