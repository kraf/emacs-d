(require 'evil)
(require 'evil-surround)
(require 'evil-nerd-commenter)
;; (require 'evil-magit)
(require 'evil-collection)
(require 'treemacs-evil)
(require 'company)
(require 'undo-fu)

(evil-mode 1)

(evil-set-undo-system 'undo-fu)

(evil-collection-init)

(global-evil-surround-mode 1)

(global-set-key (kbd "Ö") (kbd "<escape>"))

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

(setq avy-all-windows nil)
(setq avy-word-punc-regexp nil)

;; NORMAL MODE
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",q" 'evil-delete-buffer)
(define-key evil-normal-state-map "\C-k" 'paredit-kill)
(define-key evil-normal-state-map "K" 'evil-previous-line)
(define-key evil-normal-state-map ",c" 'evilnc-comment-or-uncomment-lines)
;; (define-key evil-normal-state-map ",e" 'er/expand-region)
;; (define-key evil-normal-state-map ",e" 'flycheck-next-error)
(define-key evil-normal-state-map ",,c" 'evil-avy-goto-char)
(define-key evil-normal-state-map ",,w" 'evil-avy-goto-word-0)
(define-key evil-normal-state-map ",,l" 'evil-avy-goto-line)
(define-key evil-normal-state-map ",zz" 'zoom-window-zoom)

;; GIT
(define-key evil-normal-state-map ",gs" 'magit-status)
(define-key evil-normal-state-map ",gg" 'git-gutter+-show-hunk-inline-at-point)
(define-key evil-normal-state-map ",gx" 'git-gutter+-revert-hunk)
(define-key evil-normal-state-map ",gb" 'magit-blame-addition)
(define-key evil-normal-state-map ",gt" 'git-timemachine)
(define-key evil-normal-state-map ",gl" 'git-link)

;; LSP
;; (define-key evil-normal-state-map ",ld" 'lsp-ui-peek-find-definitions)
;; (define-key evil-normal-state-map ",lr" 'lsp-ui-peek-find-references)
;; (define-key evil-normal-state-map ",ls" 'lsp-ui-peek-find-workspace-symbol)
(define-key evil-normal-state-map ",lt" 'lsp-treemacs-symbols)
(define-key evil-normal-state-map ",lf" 'lsp-treemacs-quick-fix)
(define-key evil-normal-state-map ",ln" 'lsp-rename)
(define-key evil-normal-state-map ",la" 'lsp-execute-code-action)
(define-key evil-normal-state-map ",ll" 'lsp-clojure-add-missing-libspec)
(define-key evil-normal-state-map ",lr" 'lsp-find-references)
(define-key evil-normal-state-map "gd" 'lsp-find-definition)
(define-key evil-normal-state-map "gh" 'lsp-describe-thing-at-point)

;; Multiple cursors
(define-key evil-normal-state-map "\C-d" 'mc/mark-next-like-this-word)
(define-key evil-normal-state-map ",mp" 'mc/mark-previous-like-this-word)
(define-key evil-normal-state-map ",ma" 'mc/mark-all-like-this)
(define-key evil-visual-state-map "\C-d" 'mc/mark-next-like-this)
(define-key evil-visual-state-map ",mp" 'mc/mark-previous-like-this)
(define-key evil-visual-state-map ",ma" 'mc/mark-all-like-this)

(define-key evil-normal-state-map ",rli" 'cljr-introduce-let)
(define-key evil-normal-state-map ",rle" 'cljr-expand-let)

(define-key evil-normal-state-map ",ff" 'treemacs-select-window)

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

(use-package evil-owl
	   :custom
	   (evil-owl-display-method 'posframe)
	   (evil-owl-extra-posfram-args '(:width 50 :height 20))
	   (evil-owl-idle-delay 0)
	   :init
	   (evil-owl-mode))

(use-package evil-matchit)

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
