;; Evil and friends. Keybindings are declared with general.el; the leader
;; key is "," (see `my-leader-def'). Mode-specific leader bindings live in
;; the language files (setup-clojure.el, setup-lisp.el, ...).

(use-package evil
  :init
  ;; Must be set before evil loads.
  (setq evil-want-integration t
        evil-want-keybinding nil ; evil-collection handles mode keybindings
        evil-toggle-key "M-z")
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)
  (add-to-list 'evil-emacs-state-modes 'eshell-mode))

(use-package general
  :config
  (general-create-definer my-leader-def
    :prefix ","))

(use-package evil-collection
  :after evil
  :config
  ;; lispyville owns the lispy integration
  (setq evil-collection-mode-list (delq 'lispy evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (add-hook 'evil-surround-mode-hook
            (lambda ()
              (push '(?ä . ("[" . "]")) evil-surround-pairs-alist)))
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))

(use-package evil-nerd-commenter
  :after evil)

(use-package evil-matchit
  :after evil)

(use-package evil-mc
  :after evil)

(use-package evil-owl
  :custom
  (evil-owl-display-method 'posframe)
  (evil-owl-extra-posframe-args '(:width 50 :height 20))
  (evil-owl-idle-delay 0)
  :init
  (evil-owl-mode))

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-word-punc-regexp nil))

(use-package expand-region)

(use-package zoom-window)

;;; Leader bindings (normal state)

(my-leader-def
  :states 'normal
  "w" 'save-buffer
  "q" 'evil-delete-buffer
  "c" 'evilnc-comment-or-uncomment-lines
  "." 'evil-avy-goto-char
  "f" 'treemacs-select-window
  "=" 'lsp-format-buffer

  "," '(:ignore t :which-key "avy")
  ",c" 'evil-avy-goto-char
  ",w" 'evil-avy-goto-word-1
  ",l" 'evil-avy-goto-line

  "z" '(:ignore t :which-key "zoom")
  "zz" 'zoom-window-zoom

  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gg" 'diff-hl-show-hunk
  "gx" 'diff-hl-revert-hunk
  "gb" 'magit-blame-addition
  "gt" 'git-timemachine
  "gl" 'git-link

  "l" '(:ignore t :which-key "lsp")
  "lt" 'lsp-treemacs-symbols
  "lf" 'lsp-treemacs-quick-fix
  "ln" 'lsp-rename
  "la" 'lsp-execute-code-action
  "lr" 'lsp-find-references

  "s" '(:ignore t :which-key "search")
  "sr" 'consult-ripgrep
  "sl" 'consult-line
  "si" 'consult-imenu)

;;; Leader bindings (visual state)

(my-leader-def
  :states 'visual
  "c" 'evilnc-comment-or-uncomment-lines
  "a" 'align-regexp
  "=" 'lsp-format-region)

;;; State bindings

(general-define-key
 :states 'normal
 "K" 'evil-previous-line
 "gd" 'lsp-find-definition
 "gh" 'lsp-describe-thing-at-point
 "C-z" 'er/expand-region
 "C-w C-w" 'tear-off-window
 ;; keep the emacs originals (xref jumps)
 "M-." nil
 "M-," nil)

(general-define-key
 :states 'visual
 "C-z" 'er/expand-region
 "P" (lambda ()
       (interactive)
       (evil-paste-from-register ?0)))

(general-define-key
 :states 'insert
 "C-e" 'move-end-of-line
 "C-z" 'er/expand-region
 "M-." 'yas-expand)

;; Ctrl-g should act like Esc
(defun evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

(general-define-key
 :keymaps '(evil-normal-state-map
            evil-motion-state-map
            evil-insert-state-map
            evil-window-map
            evil-operator-state-map)
 "C-g" 'evil-keyboard-quit)

;;; Globals

(global-set-key (kbd "Ö") (kbd "<escape>"))
(global-set-key (kbd "C-z") 'er/expand-region)

;; evil-toggle-key is M-z, so free the global binding (zap-to-char)
(global-unset-key (kbd "M-z"))
