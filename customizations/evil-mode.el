(require 'evil)
(require 'evil-surround)
(require 'evil-nerd-commenter)

(evil-mode 1)

(global-evil-surround-mode 1)

(global-set-key (kbd "Ö") (kbd "<escape>"))

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",q" 'evil-delete-buffer)
(define-key evil-insert-state-map "\C-e" 'move-end-of-line)
(define-key evil-normal-state-map "\C-k" 'paredit-kill)
(define-key evil-insert-state-map "\C-k" 'paredit-kill)
(define-key evil-normal-state-map "K" 'evil-previous-line)
(define-key evil-normal-state-map ",c" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map ",c" 'evilnc-comment-or-uncomment-line)
(define-key evil-normal-state-map ",e" 'er/expand-region)

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
