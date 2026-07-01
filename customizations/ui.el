;; Look and feel.

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(global-display-line-numbers-mode)
(column-number-mode 1)

(use-package doom-modeline
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-height 23)
  :config
  (doom-modeline-mode 1))

(use-package minions
  :config
  (minions-mode 1))

(use-package eyebrowse
  :config
  (eyebrowse-mode 1)
  (eyebrowse-setup-opinionated-keys))

;; Sticky header showing the top-level form point is in.
(use-package topsy
  :hook (prog-mode . topsy-mode))

(defun my/style-lsp-symbol-highlights ()
  "Style LSP symbol occurrence highlights."
  (dolist (face '(lsp-face-highlight-textual
                  lsp-face-highlight-read
                  lsp-face-highlight-write))
    (when (facep face)
      (set-face-attribute face nil
                          :foreground "#c0caf5"
                          :background "#3d59a1"
                          :weight 'unspecified
                          :underline nil
                          :overline nil
                          :strike-through nil
                          :box nil
                          :inverse-video nil
                          :extend nil))))

(use-package tokyonight-themes
  :straight (tokyonight-themes
             :type git
             :host github
             :repo "xuchengpeng/tokyonight-themes")
  :config
  (load-theme 'tokyonight-night t)
  (tokyonight-themes-with-colors
    (set-face-attribute 'line-number-current-line nil :foreground orange))
  (my/style-lsp-symbol-highlights)
  (with-eval-after-load 'lsp-mode
    (my/style-lsp-symbol-highlights)))

(set-face-attribute 'default nil :height 130)

(setq ;; makes killing/yanking interact with the clipboard
 select-enable-clipboard t

 ;; Save clipboard strings into kill ring before replacing them,
 ;; so a selection from another program survives a kill in Emacs.
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos.
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

(setq split-height-threshold nil)
(setq split-width-threshold 140)

(use-package diff-hl
  :config
  (global-diff-hl-mode 1))
