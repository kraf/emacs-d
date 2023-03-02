;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(require 'paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(define-key paredit-mode-map (kbd "RET") nil)
(define-key paredit-mode-map (kbd "C-j") 'paredit-newline)

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
