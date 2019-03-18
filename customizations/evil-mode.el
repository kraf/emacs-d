(require 'evil)
(require 'evil-surround)
(require 'evil-nerd-commenter)
(require 'evil-collection)
(require 'company)

(setq evil-want-integration t)
(setq evil-want-keybinding nil)

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
(define-key evil-normal-state-map ",e" 'er/expand-region)
(define-key evil-normal-state-map ",f" 'flycheck-next-error)
(define-key evil-normal-state-map ",g" 'magit-status)

(define-key evil-normal-state-map ",td" 'tide-documentation-at-point)
(define-key evil-normal-state-map ",tf" 'tide-jump-to-definition)
(define-key evil-normal-state-map ",tt" 'tide-jump-back)
(define-key evil-normal-state-map ",tr" 'tide-rename-symbol)
(define-key evil-normal-state-map ",tl" 'tide-references)

;; INSERT MODE
(define-key evil-insert-state-map "\C-e" 'move-end-of-line)

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

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
            (define-key evil-normal-state-local-map (kbd "x") 'neotree-enter-horizontal-split)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(setq evil-symbol-word-search 'symbol)

(add-to-list 'evil-emacs-state-modes 'eshell-mode)
(add-to-list 'evil-emacs-state-modes 'dired-mode)
