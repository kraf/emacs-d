;; Evil and friends. Leader key is ",".

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

(global-set-key (kbd "Ö") (kbd "<escape>"))
(global-set-key (kbd "C-z") 'er/expand-region)

;; evil-toggle-key is M-z, so free the global binding (zap-to-char)
(global-unset-key (kbd "M-z"))

;; NORMAL MODE
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",q" 'evil-delete-buffer)
(define-key evil-normal-state-map "K" 'evil-previous-line)
(define-key evil-normal-state-map ",c" 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map ",." 'evil-avy-goto-char)
(define-key evil-normal-state-map ",,c" 'evil-avy-goto-char)
(define-key evil-normal-state-map ",,w" 'evil-avy-goto-word-1)
(define-key evil-normal-state-map ",,l" 'evil-avy-goto-line)
(define-key evil-normal-state-map ",zz" 'zoom-window-zoom)

;; Expand
(define-key evil-insert-state-map "\C-z" 'er/expand-region)
(define-key evil-normal-state-map "\C-z" 'er/expand-region)
(define-key evil-visual-state-map "\C-z" 'er/expand-region)

;; GIT
(define-key evil-normal-state-map ",gs" 'magit-status)
(define-key evil-normal-state-map ",gg" 'diff-hl-show-hunk)
(define-key evil-normal-state-map ",gx" 'diff-hl-revert-hunk)
(define-key evil-normal-state-map ",gb" 'magit-blame-addition)
(define-key evil-normal-state-map ",gt" 'git-timemachine)
(define-key evil-normal-state-map ",gl" 'git-link)

;; LSP
(define-key evil-normal-state-map ",lt" 'lsp-treemacs-symbols)
(define-key evil-normal-state-map ",lf" 'lsp-treemacs-quick-fix)
(define-key evil-normal-state-map ",ln" 'lsp-rename)
(define-key evil-normal-state-map ",la" 'lsp-execute-code-action)
(define-key evil-normal-state-map ",lr" 'lsp-find-references)
(define-key evil-normal-state-map "gd" 'lsp-find-definition)
(define-key evil-normal-state-map "gh" 'lsp-describe-thing-at-point)

(define-key evil-normal-state-map ",=" 'lsp-format-buffer)
(define-key evil-visual-state-map ",=" 'lsp-format-region)

(define-key evil-normal-state-map ",f" 'treemacs-select-window)

(define-key evil-normal-state-map "\C-w\C-w" 'tear-off-window)

;; INSERT MODE
(define-key evil-insert-state-map "\C-e" 'move-end-of-line)
(define-key evil-insert-state-map "\M-." 'yas-expand)

;; emacs original
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)

;; VISUAL MODE
(define-key evil-visual-state-map ",c" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map ",a" 'align-regexp)
(define-key evil-visual-state-map "P" (lambda ()
                                        (interactive)
                                        (evil-paste-from-register ?0)))

;; Ctrl-g should act like Esc
(defun evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

(define-key evil-normal-state-map   (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-motion-state-map   (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-insert-state-map   (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-window-map         (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-operator-state-map (kbd "C-g") #'evil-keyboard-quit)
