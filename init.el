;;;;
;; Packages
;;;;
(setq gc-cons-threshold (* 100 1000 1000))

(load-file "~/.emacs.d/customizations/local-before.el")

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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
     
     paredit
     smartparens
     lispy
     highlight-parentheses
     clojure-mode
     clojure-mode-extra-font-locking
     cider
     zprint-mode

     ido-completing-read+

     ;; Enhances M-x to allow easier execution of commands. Provides
     ;; a filterable list of possible commands in the minibuffer
     ;; http://www.emacswiki.org/emacs/Smex
     smex

     exec-path-from-shell
     add-node-modules-path

     ag
    
     undo-fu

     magit
    
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
     browse-at-remote

     company
     company-posframe
     flx-ido
     ido-vertical-mode

     web-mode
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
     tagedit
     magit

     ;; themes
     cyberpunk-theme
     tramp-theme
     vscode-dark-plus-theme
    
     ;; LSP
     lsp-mode
     ;; lsp-ui
     ;; company-lsp
     lsp-treemacs

     tree-sitter
     tree-sitter-langs

     dired-git-info
     ranger
    
     yasnippet
     yasnippet-snippets
     multiple-cursors

     which-key))

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

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (cider-repl-mode . lispy-mode))
  :custom
  (lispy-close-quotes-at-end-p t)
  :config
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)
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

  (lispyville--define-key 'normal (kbd "M-J") #'evil-multiedit-match-and-next)
  (lispyville--define-key 'normal (kbd "M-K") #'evil-multiedit-match-and-prev)
  
  (define-key key-translation-map (kbd "ö") nil)
  (define-key key-translation-map (kbd "ä") nil)
  
  (lispyville--define-key 'normal ",eb" 'cider-eval-buffer)
  (lispyville--define-key 'normal ",ef" 'cider-eval-defun-at-point)
  (lispyville--define-key 'normal ",ee" 'cider-eval-sexp-at-point)
  (lispyville--define-key 'normal ",en" 'cider-eval-ns-form)
  (lispyville--define-key 'normal ",el" 'cider-eval-list-at-point)
  (lispyville--define-key 'visual ",ee" 'cider-insert-region-in-repl)
  
  (lispyville--define-key 'normal ",c" 'lispyville-comment-or-uncomment-line)
  (lispyville--define-key 'visual ",c" 'lispyville-comment-or-uncomment)
  ;; (lispyville--define-key 'insert (kbd "M-n") 'lispy-forward)
  ;; (lispyville--define-key 'insert (kbd "M-p") 'lispy-backward)
  (lispyville--define-key 'insert (kbd "ö") 'lispy-braces)
  (lispyville--define-key 'insert (kbd "ä") 'lispy-brackets)
  (lispyville--define-key 'normal (kbd "ö") 'lispyville-next-opening)
  (lispyville--define-key 'normal (kbd "C-ö") 'lispyville-previous-opening)
  (lispyville--define-key 'normal (kbd "ä") 'lispyville-next-closing)
  (lispyville--define-key 'normal (kbd "C-ä") 'lispyville-previous-closing)
  (lispyville--define-key '(insert normal) (kbd "M-ö") 'lispy-wrap-braces)
  (lispyville--define-key '(insert normal) (kbd "M-ä") 'lispy-wrap-brackets)

  (define-key lispy-mode-map-lispy "[" nil)
  (define-key lispy-mode-map-lispy "]" nil)
  
  (lispyville--define-key 'normal ",ir" (lambda ()
                                          (interactive)
                                          (cider-interactive-eval "(user/reset)")))
  
  (lispyville--define-key 'normal (kbd "(") (lambda () (interactive) (avy-goto-char ?\()))
  (lispyville--define-key 'normal (kbd ")") (lambda () (interactive) (avy-goto-char ?\))))
  )

(use-package undo-fu)

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
