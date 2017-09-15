;;;;
;; Packages
;;;;

(if (file-exists-p "customizations/local-before.el")
    (load-file "customizations/local-before.el"))

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    exec-path-from-shell

    neotree
    ag
    evil
    evil-surround
    evil-nerd-commenter
    expand-region
    company
    winring
    
    less-css-mode
    web-mode
    js2-mode
    tide
    
    flycheck
    projectile
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit
    magit
    vkill

    ;; themes
    cyberpunk-theme
    tramp-theme
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

(load "evil-mode.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-go.el")
(load "setup-c.el")

(load "orgmode.el")

(load "magit-custom.el")
(load "setup-company.el");

(load "term.el")

(if (file-exists-p "customizations/local.el")
    (load-file "customizations/local.el"))

(add-to-list 'load-path "~/.emacs.d/vendor")
(load "prettier-js.el")

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'prettier nil t)))

(defun prettier-if-jsx ()
  (when (equal web-mode-content-type "jsx") (prettier)))

(add-hook 'web-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'prettier-if-jsx nil t)))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments
   (quote
    ("--smart-case" "--stats" "--ignore-dir" "node_modules")))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-idle-delay 0.1)
 '(custom-safe-themes
   (quote
    ("7ef8e5ca28fa635396e37569b75772d07157e93a044987538186e9048b301151" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "ffe39e540469ef05808ab4b75055cc81266875fa4a0d9e89c2fec1da7a6354f3" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "7a00b0710bb2e400d33a925f94b1cd8cfa2281f864ac9506b9046703e0045d66" "0eebf69ceadbbcdd747713f2f3f839fe0d4a45bd0d4d9f46145e40878fc9b098" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" default)))
 '(delete-selection-mode nil)
 '(evil-toggle-key "M-z")
 '(fci-rule-color "#2a2a2a")
 '(flycheck-locate-config-file-functions (quote (flycheck-locate-config-file-ancestor-directories)))
 '(magit-pull-arguments (quote ("--rebase")))
 '(package-selected-packages
   (quote
    (company-irony ahungry-theme foggy-night-theme tramp-theme company-go go-mode restclient js2-mode tide docker zone-select zone-rainbow zone-nyan yaml-mode winring web-mode vkill toml-mode tern tagedit symon smex scss-mode sass-mode relative-line-numbers rainbow-delimiters projectile php-mode paredit nyan-mode nginx-mode neotree markdown-mode magit less-css-mode kv jade-mode ido-ubiquitous expand-region exec-path-from-shell evil-surround evil-nerd-commenter cyberpunk-theme company coffee-mode clojure-mode-extra-font-locking cider aggressive-indent ag)))
 '(prettier-args (quote ("--single-quote" "--tab-width" "4")))
 '(tramp-default-method "ssh"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
