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
  (read-process-output-max (* 1024 1024)))

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
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments nil)
 '(ag-reuse-buffers t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-backends '(company-capf))
 '(company-echo-delay nil t)
 '(company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-require-match 'never)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 14)
 '(custom-safe-themes
   '("cf861f5603b7d22cb3545a7c63b2ee424c34d8ed3b3aa52d13abfea4765cffe7" "b9a67b48d56c580cb300ce9c1ecc3b83aee953346a33e2a14b31e2e4a07ea8a6" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "824d07981667fd7d63488756b6d6a4036bae972d26337babf7b56df6e42f2bcd" "6ebdb33507c7db94b28d7787f802f38ac8d2b8cd08506797b3af6cdfd80632e0" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "dfe0523e20114df987a41afb6ac5698307e65e0fcb9bff12dc94621e18d44c3d" "4561c67b0764aa6343d710bb0a6f3a96319252b2169d371802cc94adfea5cfc9" "7ef8e5ca28fa635396e37569b75772d07157e93a044987538186e9048b301151" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "ffe39e540469ef05808ab4b75055cc81266875fa4a0d9e89c2fec1da7a6354f3" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "7a00b0710bb2e400d33a925f94b1cd8cfa2281f864ac9506b9046703e0045d66" "0eebf69ceadbbcdd747713f2f3f839fe0d4a45bd0d4d9f46145e40878fc9b098" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" default))
 '(delete-selection-mode nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(evil-owl-display-method 'posframe)
 '(evil-owl-extra-posfram-args '(:width 50 :height 20) t)
 '(evil-owl-idle-delay 0)
 '(evil-toggle-key "M-z")
 '(fci-rule-color "#2a2a2a")
 '(flycheck-locate-config-file-functions '(flycheck-locate-config-file-ancestor-directories))
 '(lsp-auto-guess-root nil)
 '(lsp-eldoc-hook nil)
 '(lsp-file-watch-threshold 2000)
 '(lsp-keymap-prefix "s-i")
 '(lsp-prefer-flymake nil t)
 '(lsp-ui-doc-max-width 80)
 '(lsp-ui-doc-position 'top)
 '(magit-pull-arguments '("--rebase"))
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(treemacs-evil add-node-modules-path kubernetes typescript vscode-dark-plus-theme monokai-theme dracula-theme rainbow-mode gruvbox-theme tree-sitter tree-sitter-langs clj-refactor which-key company-box company-quickhelp evil-cleverparens zprint-mode npm-mode browse-at-remote egg-timer yaml-mode flymake-eslint restclient git-timemachine typescript-mode rjsx-mode multiple-cursors yasnippet ranger dired-git-info tramp-theme cyberpunk-theme treemacs rainbow-delimiters projectile flycheck prettier-js web-mode ido-vertical-mode flx-ido company-posframe company git-gutter-fringe+ evil-magit evil-matchit evil-collection evil-owl evil-nerd-commenter evil-surround evil ag exec-path-from-shell smex ido-completing-read+ cider clojure-mode-extra-font-locking clojure-mode paredit use-package))
 '(read-process-output-max 1048576 t)
 '(safe-local-variable-values
   '((cider-refresh-before-fn . "user/reset")
     (eval lispyville--define-key 'normal
           (kbd ",ir")
           (lambda nil
             (interactive)
             (cider-nrepl-sync-request:eval "(user/reset)")))))
 '(select-enable-clipboard nil)
 '(select-enable-primary nil)
 '(tramp-completion-reread-directory-timeout 0)
 '(tramp-default-method "scp")
 '(tramp-remote-path
   '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin" tramp-own-remote-path))
 '(tramp-verbose 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
