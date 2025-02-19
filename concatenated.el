;; customizations/setup-ruby.el
(add-hook 'ruby-mode-hook
          (lambda ()
            (evil-matchit-mode)
            (electric-pair-mode)
            (electric-indent-mode)
            ;; (lsp)
            ))

;; customizations/elisp-editing.el
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

;; customizations/setup-clojure.el
;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
(require 'cider)
(require 'zprint)

;; (require 'flycheck-clj-kondo)
;; (setq flycheck-clj-kondo-clj-executable (executable-find "clj-kondo"))
;; (setq flycheck-clj-kondo-cljs-executable (executable-find "clj-kondo"))
;; (setq flycheck-clj-kondo-edn-executable (executable-find "clj-kondo"))
;; (setq flycheck-clj-kondo-cljc-executable (executable-find "clj-kondo"))

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

(defun clojure-hook ()
  ;; (add-to-list 'flycheck-checkers 'flycheck-clj-kondo)
  ;; (setq inferior-lisp-program "lein repl")
  (font-lock-add-keywords
   nil
   '(("(\\(facts?\\)"
      (1 font-lock-keyword-face))
     ("(\\(background?\\)"
      (1 font-lock-keyword-face))))
  ;; (flycheck-mode)
  (multiple-cursors-mode)
  (zprint-mode)
  (highlight-parentheses-mode)
  (clj-refactor-mode)
  ;; (define-clojure-indent (fact 1))
  ;; (define-clojure-indent (facts 1))
  (setq evil-symbol-word-search t)
  (setq cljr-insert-newline-after-require nil)

  (lispy-mode)
  (lispyville-mode)

  (prettify-symbols-mode)

  ;; Fix clojure-lsp conflicts with other modes
  (setq cljr-add-ns-to-blank-clj-files nil)
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq lsp-keep-workspace-alive nil)

  (setq-local lsp-idle-delay 0.2)
  (lsp)

  ;; (setq completion-at-point-functions '(cider-complete-at-point))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t)

  (setq cider-format-code-options
        '(
          ("indents" (("plait" (("inner" 0)))
                      ("match" (("inner" 0)))))
          ))

  ;; (setq-local completion-at-point-functions '(codeium-completion-at-point))

  (define-clojure-indent
    (match 1)))



;; syntax hilighting for midje
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'clojurescript-mode-hook 'clojure-hook)

;; enable paredit in your REPL
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;;;;
;; Cider
;;;;

(use-package cider
  :after (lispyville)
  :commands cider
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-result-prefix ";; => ")

  ;; When there's a cider error, show its buffer and switch to it
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Wrap when navigating history.
  (cider-repl-wrap-history t)

  (local-set-key (kbd "C-c C-e") 'cider-eval-sexp-at-point)

  :config
  (add-hook 'cider-repl-mode-hook 'evil-insert-state)

  (add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion)

  (lispyville--define-key 'normal ",eb" 'cider-eval-buffer)
  (lispyville--define-key 'normal ",ef" 'cider-eval-defun-at-point)
  (lispyville--define-key 'normal ",ee" 'cider-eval-sexp-at-point)
  (lispyville--define-key 'normal ",en" 'cider-eval-ns-form)
  (lispyville--define-key 'normal ",el" 'cider-eval-list-at-point)
  (lispyville--define-key 'visual ",ee" 'cider-insert-region-in-repl))

;; customizations/setup-python.el
(require 'company)
(require 'jedi)

(setq flycheck-python-pylint-executable "~/anaconda2/bin/pylint")

(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode)
                              (electric-pair-mode)
                              (electric-indent-mode)
                              (jedi-mode)
                              (indent-guide-mode)
                              (modify-syntax-entry ?_ "w")
                              (add-hook 'before-save-hook 'whitespace-cleanup)
                              (add-to-list (make-local-variable 'company-backends)
                                           '(company-jedi company-keywords))
                              ))

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; customizations/miscellaneous.el
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts (setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(setq ag-highlight-search t)

(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

(setq confirm-kill-emacs 'y-or-n-p)

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;; (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

;; (tramp-enable-method "distrobox")

;; customizations/setup-c.el
(require 'company)

(add-hook 'c++-mode-hook (lambda ()
                           (electric-pair-mode)
                           (electric-indent-mode)
                           (flycheck-mode)
                           (add-to-list (make-local-variable 'company-backends)
                                        '(company-gtags
                                          company-keywords
                                          company-dabbrev))
                           (modify-syntax-entry ?_ "w")
                           ))

(add-to-list 'auto-mode-alist '("\\.cuh?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

;; customizations/terminal.el
(defun silence ()
  (interactive))

(if (not (display-graphic-p))
    (progn
      (xterm-mouse-mode)
      (set-face-background 'region "#81660a")
      (define-key evil-motion-state-map [down-mouse-1] nil)
      (define-key evil-motion-state-map [mouse-1] nil)
))

;; customizations/magit-custom.el
(require 'magit)
;; (setq magit-last-seen-setup-instructions "1.4.0")
;; (setq git-commit-summary-max-length 79)

;; (require 'evil-magit)

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(with-eval-after-load 'magit
  ;; (require 'forge)
  (define-key evil-visual-state-map "s" 'magit-stage))

;; customizations/local-before.el
;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 3129 5))

;; customizations/setup-company.el
(setq company-dabbrev-downcase nil)
;; (setq company-backends
;;       (quote
;;        (company-nxml company-css company-eclim company-semantic company-clang company-cmake company-files company-capf
;;                      (company-dabbrev-code company-keywords)
;;                      company-oddmuse company-dabbrev)))
(setq company-backends
      '((;; company-files ;; disabled because sometimes really slow
         company-keywords
         company-capf
         company-yasnippet
         company-abbrev
         company-dabbrev)))

(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)

;; customizations/editing.el
;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
(setq require-final-newline t)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)
(setq require-final-newline t)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map (kbd "C-.") 'company-complete)

; (setq mac-option-modifier 'meta)
;; (setq mac-command-modifier 'meta)
(setq mac-right-option-modifier nil)
(setq mac-right-command-modifier nil)

;; (global-set-key (kbd "A-7") (kbd "{"))
;; (global-set-key (kbd "A-8") (kbd "["))
;; (global-set-key (kbd "A-9") (kbd "]"))
;; (global-set-key (kbd "A-0") (kbd "}"))
;; (global-set-key (kbd "A-<") (kbd "|"))
;; (global-set-key (kbd "A-+") (kbd "~"))
;; (global-set-key (kbd "A-ß") (kbd "\\"))
;; (global-set-key (kbd "A-q") (kbd "@"))

(require 'yasnippet)
(yas-global-mode 1)

(require 'multiple-cursors)
(setq mc/always-repeat-command 1)
(setq mc/always-run-for-all 1)

(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))

(setq projectile-create-missing-test-files t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; customizations/local.el
(set-face-attribute 'default nil :font "Hack" :height 130)
;; (set-face-attribute 'default nil :font "Fira Code" :height 130)

(require 'treemacs)
(add-hook 'treemacs-mode-hook
          (lambda ()
            (face-remap-add-relative 'default
                                     :family "Ubuntu"
                                     :height 120
                                     :weight 'normal)
            (treemacs-resize-icons 16)
            (setq treemacs-width 26)))

;; (setq initial-buffer-choice "~/Synced/Vega/orgs/focus.org")

;; customizations/setup-js.el
(require 'flycheck)
(require 'prettier-js)
;; (require 'js2-refactor)
(require 'web-mode)

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; Create a derived mode from web-mode
(define-derived-mode vue-mode web-mode "VueJS"
 "Extend web-mode to .vue files")
(provide 'vue-mode)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; (js2r-add-keybindings-with-prefix "C-c C-m")

(setq flycheck-disabled-checkers '(javascript-jshint))
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; (setq lsp-eslint-server-command
;;       '("node"
;;         ;; "/home/filip/src/github/vscode-eslint/server/out/eslintServer.js"
;;         "/home/filip/.vscode/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js"
;;         "--stdio"))

;; (defun my/ensure-curly-square-shortcut ()
;;   (define-key key-translation-map (kbd "ö") (kbd "{"))
;;   (define-key key-translation-map (kbd "ä") (kbd "["))
;;   )

(add-hook 'web-mode-hook
          (lambda ()
            (add-node-modules-path)

            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?' . ?') (?` . ?`))))

            (electric-pair-mode)
            (electric-indent-mode)
            (emmet-mode)

            (evil-matchit-mode)

            ;; (when (string-match "vue" (file-name-extension buffer-file-name))
            ;;   (flycheck-mode)
            ;;   (prettier-js-mode)
            ;;   ;; (setq-local lsp-enabled-clients '(vue-semantic-server))
            ;;   ;; (lsp)
            ;;   (flycheck-add-next-checker 'lsp 'javascript-eslint)
            ;;   )
          ))

(add-hook 'vue-mode-hook
          (lambda ()
            (add-node-modules-path)

            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?' . ?') (?` . ?`))))

            (electric-pair-mode)
            (electric-indent-mode)
            (emmet-mode)

            (evil-matchit-mode)

            (flycheck-mode)
            (prettier-js-mode)

            (lsp-dependency 'typescript
                            '(:npm :package "typescript"
                                   :path "tsserver"))

            (lsp)
            (flycheck-add-next-checker 'lsp 'javascript-eslint)
            ))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (flycheck-mode)
            (electric-pair-mode)
            (electric-indent-mode)
            (npm-mode)
            (evil-matchit-mode)

            (setq-local sgml-basic-offset 2)
            (setq-local js2-basic-offset 2)
            (setq-local js2-strict-missing-semi-warning nil)
            (setq-local js2-strict-inconsistent-return-warning nil)

            ;; (my/ensure-curly-square-shortcut)

            (add-node-modules-path)
            (prettier-js-mode)
            (setq-local lsp-disabled-clients '(vue-semantic-server))
            (lsp)
            (flycheck-add-next-checker 'lsp 'javascript-eslint)
            ;; (flycheck-add-next-checker 'lsp '(t . javascript-eslint))
            (setq-local company-backends '(company-capf))))

(add-hook 'css-mode-hook (lambda ()
                           (add-node-modules-path)
                           (prettier-js-mode)))

(add-hook 'scss-mode-hook (lambda ()
                            (add-node-modules-path)
                            (prettier-js-mode)))

;; FIXME: this is not working, why?!?
(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?$"
  :hook
  ((typescript-mode . (lambda ()
                        (setq typescript-indent-level 2)
                        (add-node-modules-path)
                        (lsp)
                        (tree-sitter-hl-mode)
                        ;; (my/ensure-curly-square-shortcut)
                        (prettier-js-mode)))))

(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)
;; (use-package prettier-js :ensure t :hook (typescript-mode))

;; couldn't make it work with `use-package`, plain elisp instead
;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)
;; (add-hook 'typescript-mode-hook (lambda ()
;;                                   (tree-sitter-hl-mode)
;;                                   (lsp)))

;; customizations/ui.el
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

(doom-modeline-mode)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-height 23)

(minions-mode)

(eyebrowse-mode)
(eyebrowse-setup-opinionated-keys)

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
(load-theme 'vscode-dark-plus t)
;; (load-theme 'tramp t)
;; (load-theme 'twilight-bright)

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

(global-diff-hl-mode)

;; customizations/my-exwm.el
(server-start)

(setq display-time-default-load-average nil)
(display-time-mode t)
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
                    'face 'egoge-display-time)))

(require 'exwm)
(require 'exwm-config)

(exwm-config-ido)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 10)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,(- i 1)))))
                  (number-sequence 1 9))
        
        ;; Bind "s-0 0" to "s-0 9" to 10 - 11
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-0 %d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,(+ i 9)))))
                  (number-sequence 0 9))
        
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-o] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; Bind "s-l" to "slock", a simple X display locker.
        ([?\s-l] . (lambda ()
                     (interactive)
                     (start-process "" nil "/usr/bin/slock")))))

;; Set global EXWM key bindings
(exwm-input-set-key (kbd "s-t") #'exwm-floating-toggle-floating)
(exwm-input-set-key (kbd "s-s") #'exwm-workspace-switch-to-buffer)
(exwm-input-set-key (kbd "s-m") #'exwm-workspace-move-window)
(exwm-input-set-key (kbd "s-z") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "s-<f2>") #'smex)
(exwm-input-set-key (kbd "s-<return>") (lambda () 
                                         (interactive)
                                         (start-process-shell-command "Terminal" nil "gnome-terminal")))

(exwm-input-set-key (kbd "s-v") (lambda () 
                                  (interactive)
                                  (select-window (split-window-vertically))
                                  (balance-windows)))

(exwm-input-set-key (kbd "s-h") (lambda () 
                                  (interactive)
                                  (select-window (split-window-horizontally))
                                  (balance-windows)))

(exwm-input-set-key (kbd "s-q") #'delete-window)
(exwm-input-set-key (kbd "s-o") (lambda (command)
                                  (interactive (list (read-shell-command "$ ")))
                                  (start-process-shell-command command nil command)))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'desktop-environment-brightness-increment)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'desktop-environment-brightness-decrement)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment)
(exwm-input-set-key (kbd "s-<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment-slowly)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement)
(exwm-input-set-key (kbd "s-<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement-slowly)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'desktop-environment-toggle-mute)

;; FIXME why doesnt this work?
;; (exwm-input-set-key (kbd "s-<f4>") (lambda ()
;;                                      (interactive)
;;                                      (kill-current-buffer)))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
;; (define-key exwm-mode-map [?\s-t] #'exwm-input-toggle-keyboard)
;; (define-key exwm-mode-map [s-Q] #'kill-this-buffer) ;; doesnt work

(setq exwm-manage-configurations '((t char-mode t)))

;; don't copy on selct
(setq select-enable-primary nil)

(display-battery-mode t)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(exwm-enable)

;; TODO
;; - how quit via keyboard?
;; - how setup default workspaces with running commands

;; customizations/setup-go.el
(require 'flycheck)

(add-to-list 'flycheck-checkers 'go-build)
(flycheck-add-mode 'go-build 'go-mode)

(add-hook 'go-mode-hook
          (lambda ()
            (electric-pair-mode)
            (electric-indent-mode)
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))


;; customizations/shell-integration.el
;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(require 'asdf)

(asdf-enable)

;; customizations/orgmode.el
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (text-scale-decrease 1)))

(setq org-default-notes-file "~/orgs/inbox.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("T" "Todo" entry (file+headline "~/orgs/inbox.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("t" "Todo" entry (file "~/Synced/orgzly/Todo.org")
         "* TODO %?\n %i")
        ("j" "Journal" entry (file+datetree "~/Synced/orgzly/Journal.org")
         "* %?\nEntered on %U\n %i")
        ("p" "People" entry (file+headline "~/orgs/people.org" "Inbox")
         "* %? %T")))

(setq org-agenda-files '("~/Synced/orgzly/Todo.org"))

;; customizations/evil-mode.el
(require 'evil)
(require 'evil-surround)
(require 'evil-nerd-commenter)
;; (require 'evil-magit)
(require 'evil-collection)
(require 'treemacs-evil)
(require 'company)

(evil-mode 1)

(evil-set-undo-system 'undo-redo)

(evil-collection-init)

(global-evil-surround-mode 1)

(global-set-key (kbd "Ö") (kbd "<escape>"))
(global-set-key (kbd "C-z") 'er/expand-region)

;; (defalias #'forward-evil-word #'forward-evil-symbol)

;; make evil-search-word look for symbol rather than word boundaries
;; (setq-default evil-symbol-word-search t)

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

(define-key evil-normal-state-map ",=" 'lsp-format-buffer)
(define-key evil-visual-state-map ",=" 'lsp-format-region)

;; Swiper
;; (define-key evil-normal-state-map "/" 'swiper)

(define-key evil-normal-state-map ",ril" 'cljr-introduce-let)
(define-key evil-normal-state-map ",rel" 'cljr-expand-let)

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
;; (define-key evil-visual-state-map ",." 'company-complete)
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

;; (setq evil-symbol-word-search 'symbol)

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

;; customizations/navigation.el
;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Fuzzy search for ivy
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)
(setq ivy-virtual-abbreviate 'abbreviate
      uniquify-min-dir-content 10)

(counsel-mode)
;; Replaced by counsel-mode
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; ;; ido-mode allows you to more easily navigate choices. For example,
;; ;; when you want to switch buffers, ido presents you with a list
;; ;; of buffers in the the mini-buffer. As you start to type a buffer's
;; ;; name, ido will narrow down the list of buffers to match the text
;; ;; you've typed in
;; ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;; (ido-mode t)
;; (ido-everywhere 1)

;; ;; Turn this behavior off because it's annoying
;; (setq ido-use-filename-at-point nil)

;; ;; Don't try to match file across all "work" directories; only match files
;; ;; in the current directory displayed in the minibuffer
;; (setq ido-auto-merge-work-directories-length -1)

;; ;; Includes buffer names of recently open files, even if they're not
;; ;; open now
;; (setq ido-use-virtual-buffers t)

;; ;; This enables ido in all contexts where it could be useful, not just
;; ;; for selecting buffer and file names
;; (require 'ido-completing-read+)
;; (ido-ubiquitous-mode 1)

;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;; (require 'ido-vertical-mode)
;; (ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
;; (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;; (smex-initialize)

(amx-mode)
(global-set-key "\C-s" 'swiper)

;; projectile everywhere!
(projectile-global-mode)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun projectile-find-file-other-window-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (find-file-other-window (projectile-completing-read "Find file in projects: " (projectile-all-project-files))))

(define-key projectile-command-map (kbd "4 F") #'projectile-find-file-other-window-in-known-projects)

;; winring
;; (require 'winring)
;; (winring-initialize)
;; (setq winring-show-names t)

(global-set-key (kbd "M-w") 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

