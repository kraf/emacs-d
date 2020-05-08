(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(require 'evil)
(require 'evil-surround)
(require 'evil-nerd-commenter)
(require 'evil-collection)
(require 'evil-magit)
(require 'company)

(evil-mode 1)

(evil-collection-init)

(global-evil-surround-mode 1)

(global-set-key (kbd "Ö") (kbd "<escape>"))

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

;; NORMAL MODE
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",q" 'evil-delete-buffer)
(define-key evil-normal-state-map "\C-k" 'paredit-kill)
(define-key evil-normal-state-map "K" 'evil-previous-line)
(define-key evil-normal-state-map ",c" 'evilnc-comment-or-uncomment-lines)
;; (define-key evil-normal-state-map ",e" 'er/expand-region)
(define-key evil-normal-state-map ",e" 'flycheck-next-error)

;; GIT
(define-key evil-normal-state-map ",gs" 'magit-status)
(define-key evil-normal-state-map ",gg" 'git-gutter+-show-hunk-inline-at-point)
(define-key evil-normal-state-map ",gm" 'magit-blame-addition)

;; LSP
(define-key evil-normal-state-map ",ld" 'lsp-ui-peek-find-definitions)
(define-key evil-normal-state-map ",lr" 'lsp-ui-peek-find-references)
(define-key evil-normal-state-map ",ls" 'lsp-ui-peek-find-workspace-symbol)
(define-key evil-normal-state-map ",lt" 'lsp-treemacs-symbols)
(define-key evil-normal-state-map ",lf" 'lsp-treemacs-quick-fix)
(define-key evil-normal-state-map ",ln" 'lsp-rename)
(define-key evil-normal-state-map ",lh" 'lsp-hover)

(define-key evil-normal-state-map ",ff" 'treemacs)
(define-key evil-normal-state-map ",fr" 'ranger)

(define-key evil-normal-state-map "\C-w\C-w" 'tear-off-window)

;; INSERT MODE
(define-key evil-insert-state-map "\C-e" 'move-end-of-line)
(define-key evil-insert-state-map "\M-." 'yas-expand)

;; VISUAL MODE
(define-key evil-visual-state-map ",c" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map ",e" 'er/expand-region)
(define-key evil-visual-state-map ",a" 'align-regexp)
(define-key evil-visual-state-map ",." 'company-complete)
(define-key evil-visual-state-map "P" (lambda ()
                                        (interactive)
                                        (evil-paste-from-register ?0)))

;; (define-key evil-insert-state-map "\C-n" 'company-select-next)
;; (define-key evil-insert-state-map "\C-p" 'company-select-previous)

;; init.el sets evil-toggle-key there, so we need to unmap global thing
(global-unset-key "\M-z")

(add-hook 'evil-surround-mode-hook
          (lambda ()
            (push '(?ä . ("[" . "]")) evil-surround-pairs-alist)))

(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key evil-insert-state-map "\C-k" 'paredit-kill)))

(setq evil-symbol-word-search 'symbol)

(add-to-list 'evil-emacs-state-modes 'eshell-mode)
(add-to-list 'evil-emacs-state-modes 'dired-mode)

(use-package evil-owl
	   :custom
	   (evil-owl-display-method 'posframe)
	   (evil-owl-extra-posfram-args '(:width 50 :height 20))
	   (evil-owl-idle-delay 0)
	   :init
	   (evil-owl-mode))

(use-package evil-matchit)
