;;;;
;; Packages
;;;;
(setq gc-cons-threshold (* 100 1000 1000))

(load-file "~/.emacs.d/customizations/local-before.el")

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Vendor
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(setq evil-want-integration t)
(setq evil-want-keybinding nil)

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(use-package

     doom-modeline
     minions

     paredit
     ;; smartparens
     lispy
     highlight-parentheses
     clojure-mode
     clojure-mode-extra-font-locking
     cider
     clj-refactor

     ;; ido-completing-read+
     ;; flx-ido
     ;; ido-vertical-mode

     ;; Enhances M-x to allow easier execution of commands. Provides
     ;; a filterable list of possible commands in the minibuffer
     amx ;; previously smex

     ;; Enhance ivy
     flx
     counsel
     swiper

     exec-path-from-shell
     add-node-modules-path

     ag

     evil
     evil-surround
     evil-nerd-commenter
     evil-owl
     evil-collection
     evil-matchit
     lispyville
     avy

     git-gutter-fringe+
     git-timemachine
     git-link
     browse-at-remote

     company
     company-posframe

     web-mode
     emmet-mode
     js2-mode
     rjsx-mode
     prettier-js
     npm-mode

     flycheck
     flycheck-clj-kondo

     projectile
     rainbow-delimiters

     treemacs
     treemacs-evil
     ;; python
     ;; jedi

     ;; edit html tags like sexps
     ;; tagedit

     magit
     forge

     ;; themes
     ;; cyberpunk-theme
     tramp-theme
     vscode-dark-plus-theme

     ;; LSP
     lsp-mode
     ;; lsp-ui
     lsp-treemacs

     tree-sitter
     tree-sitter-langs

     dired-git-info

     yasnippet
     ;; yasnippet-snippets
     ;; evil-mc
     expand-region

     which-key

     doom-modeline
     eyebrowse

     zoom-window))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(use-package git-gutter-fringe+
             :config
             (global-git-gutter+-mode)
             ;; (git-gutter-fr+-minimal)
             )

(use-package which-key
             :config
             (which-key-mode))

(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-keymap-prefix "s-i")
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (lsp-headerline-breadcrumb-enable nil)
  (read-process-output-max (* 1024 1024))
  :config
  (remove-hook 'lsp-configure-hook 'lsp-headerline-breadcrumb-mode))

;; (use-package lsp-ui
;; 	   :custom
;; 	   (lsp-ui-doc-max-width 80)
;; 	   (lsp-ui-doc-position 'top)
;;            (lsp-ui-doc-enable f))

(use-package company
             :hook (prog-mode . company-mode)
             :custom
             (company-tooltip-align-annotations t)
             (company-tooltip-limit 14)
             (company-echo-delay (if (display-graphic-p) nil 0))
             (company-minimum-prefix-length 1)
             (company-idle-delay 0.1)
             (company-require-match 'never)
             (company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
             (company-backends '(company-capf)))

(use-package company-posframe
             :config
             (company-posframe-mode))

;; (use-package smartparens
;;   :hook (prog-mode . smartparens-mode))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (cider-repl-mode . lispy-mode))
  :custom
  (lispy-close-quotes-at-end-p t)
  :config
  (define-key lispy-mode-map-lispy "[" nil)
  (define-key lispy-mode-map-lispy "]" nil))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :custom
  (lispyville-key-theme '(operators
                          c-w
                          (prettify insert)
                          additional
                          additional-insert
                          additional-movement
                          additional-wrap
                          (atom-movement normal visual)
                          slurp/barf-cp))
  :config
  (lispy-set-key-theme '(lispy c-digits))
  (lispyville-set-key-theme)

  (define-key key-translation-map (kbd "ö") nil)
  (define-key key-translation-map (kbd "ä") nil)

  (lispyville--define-key 'normal ",c" 'lispyville-comment-or-uncomment-line)
  (lispyville--define-key 'visual ",c" 'lispyville-comment-or-uncomment)
  ;; (lispyville--define-key 'insert (kbd "M-n") 'lispy-forward)
  ;; (lispyville--define-key 'insert (kbd "M-p") 'lispy-backward)
  (lispyville--define-key 'insert (kbd "{") 'lispy-braces)
  (lispyville--define-key 'insert (kbd "[") 'lispy-brackets)
  (lispyville--define-key 'normal (kbd "[") 'evil-backward-section-begin)
  (lispyville--define-key 'normal (kbd "]") 'evil-forward-section-begin)
  ;; (lispyville--define-key 'normal (kbd "{") 'lispyville-previous-opening)
  ;; (lispyville--define-key 'normal (kbd "}") 'lispyville-previous-closing)
  (lispyville--define-key '(insert normal) (kbd "M-ö") 'lispy-wrap-braces)
  (lispyville--define-key '(insert normal) (kbd "M-ä") 'lispy-wrap-brackets)
  (lispyville--define-key '(insert normal) (kbd "M-r") 'raise-sexp)

  (lispyville--define-key 'insert (kbd "C-y") 'lispy-yank)

  (lispyville--define-key 'normal "gd" 'lsp-find-definition)
  (lispyville--define-key 'normal (kbd "M-.") 'lsp-find-definition)

  (define-key lispy-mode-map-lispy "[" nil)
  (define-key lispy-mode-map-lispy "]" nil)

  (lispyville--define-key 'normal ",jc" 'lispy-clone)
  (lispyville--define-key 'normal ",jd" 'evil-collection-lispy-delete-then-next-sexp)
  (lispyville--define-key 'normal ",jr" (lambda ()
                                          (interactive)
                                          (cider-interactive-eval "(user/reset)")))
  (lispyville--define-key 'normal ",jf" (lambda ()
                                          (interactive)
                                          (cider-interactive-eval "(clojure.tools.namespace.repl/refresh)")))

  (lispyville--define-key 'normal (kbd "(") (lambda () (interactive) (avy-goto-char ?\()))
  (lispyville--define-key 'normal (kbd ")") (lambda () (interactive) (avy-goto-char ?\))))

  (lispyville--define-key 'normal "H" 'beginning-of-defun)
  (lispyville--define-key 'normal "L" 'end-of-defun))

;; (use-package codeium
;;     ;; if you use straight
;;     ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
;;     ;; otherwise, make sure that the codeium.el file is on load-path

;;     :init
;;     ;; use globally
;;     (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;     ;; or on a hook
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;     ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions
;;     ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;     ;; an async company-backend is coming soon!

;;     ;; codeium-completion-at-point is autoloaded, but you can
;;     ;; optionally set a timer, which might speed up things as the
;;     ;; codeium local language server takes ~0.2s to start up
;;     ;; (add-hook 'emacs-startup-hook
;;     ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;     ;; :defer t ;; lazy loading, if you want
;;     :config
;;     (setq use-dialog-box nil) ;; do not use popup boxes

;;     ;; if you don't want to use customize to save the api-key
;;     ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;     ;; get codeium status in the modeline
;;     (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;     (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t))

;; (use-package undo-fu)


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

(load "evil-mode.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "miscellaneous.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-ruby.el")
;; (load "setup-go.el")
;; (load "setup-c.el")
;; (load "setup-python.el")

(load "orgmode.el")

(load "magit-custom.el")
(load "setup-company.el");

(load "terminal.el")

(load "local.el")

(setq custom-file (concat user-emacs-directory ".custom.el")) ; tell Customize to save customizations to ~/.emacs.d/.custom.el
(ignore-errors                                                ; load customizations from ~/.emacs.d/.custom.el
  (load-file custom-file))
