;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Show line numbers
;;(global-relative-line-numbers-mode)
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

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; (load-theme 'tomorrow-night-bright t)

;; (if (display-graphic-p)
;;     (load-theme 'vscode-dark-plus t)
;;   (load-theme 'tango-dark t))
;; (load-theme 'vscode-dark-plus t)
;; (load-theme 'monokai-pro-spectrum t)
;; (load-theme 'tramp t)
;; (load-theme 'twilight-bright)

(use-package tokyonight-themes
  :straight (tokyonight-themes
             :type git
             :host github
             :repo "xuchengpeng/tokyonight-themes")
  :config
  (load-theme 'tokyonight-night t)
  (tokyonight-themes-with-colors
    (set-face-attribute 'line-number-current-line nil :foreground orange)

    ;; LSP symbol occurrence highlights
    (dolist (face '(lsp-face-highlight-textual
                    lsp-face-highlight-read
                    lsp-face-highlight-write))
      (set-face-attribute face nil
                          :foreground 'unspecified
                          :background 'unspecified
                          :weight 'bold
                          :underline nil
                          :box '(:line-width -1 :color "#bb9af7")))))

;; increase font size for better readability

(set-face-attribute 'default nil :height 130)

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 x-select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
;; (global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

(setq split-height-threshold nil)
(setq split-width-threshold 140)

(define-key global-map (kbd "C-x z") nil)
(define-key global-map (kbd "C-x C-z") nil)

(use-package diff-hl
  :config
  (global-diff-hl-mode 1))
