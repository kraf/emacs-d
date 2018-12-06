;;;;
;; Packages
;;;;

(load-file "~/.emacs.d/customizations/local-before.el")

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
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

    ido-completing-read+

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

    company
    flx-ido
    ido-vertical-mode

    less-css-mode
    web-mode
    js2-mode
    rjsx-mode
    tide
    prettier-js

    flycheck
    projectile
    rainbow-delimiters

    ;; python
    jedi

    ;; edit html tags like sexps
    tagedit
    magit
    evil-magit

    ;; themes
    cyberpunk-theme
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


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
(load "miscellaneous.el")

;; For editing lisps
(load "elisp-editing.el")

(load "evil-mode.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-go.el")
(load "setup-c.el")
(load "setup-python.el")

(load "evil-mode.el")
(load "orgmode.el")

(load "magit-custom.el")
(load "setup-company.el");

(load "terminal.el")

(load "local.el")

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))

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
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-idle-delay 0.1)
 '(custom-safe-themes
   (quote
    ("d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "dfe0523e20114df987a41afb6ac5698307e65e0fcb9bff12dc94621e18d44c3d" "4561c67b0764aa6343d710bb0a6f3a96319252b2169d371802cc94adfea5cfc9" "7ef8e5ca28fa635396e37569b75772d07157e93a044987538186e9048b301151" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "ffe39e540469ef05808ab4b75055cc81266875fa4a0d9e89c2fec1da7a6354f3" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "7a00b0710bb2e400d33a925f94b1cd8cfa2281f864ac9506b9046703e0045d66" "0eebf69ceadbbcdd747713f2f3f839fe0d4a45bd0d4d9f46145e40878fc9b098" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" default)))
 '(delete-selection-mode nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(evil-toggle-key "M-z")
 '(fci-rule-color "#2a2a2a")
 '(flycheck-locate-config-file-functions (quote (flycheck-locate-config-file-ancestor-directories)))
 '(magit-pull-arguments (quote ("--rebase")))
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (evil-easymotion desktop-environment exwm rjsx-mode xref-js2 company-tern ido-vertical-mode flx-ido indent-guide yapfify yaml-mode winring web-mode vkill tramp-theme tide tagedit sphinx-doc smex rainbow-delimiters protobuf-mode projectile paredit neotree less-css-mode js2-mode jinja2-mode jedi ido-ubiquitous git-gutter-fringe ggtags foggy-night-theme expand-region exec-path-from-shell evil-surround evil-nerd-commenter evil-magit cyberpunk-theme company-jedi company-irony clojure-mode-extra-font-locking cider ahungry-theme ag)))
 '(select-enable-clipboard nil)
 '(select-enable-primary nil)
 '(tramp-completion-reread-directory-timeout 0 nil (tramp))
 '(tramp-default-method "scp" nil (tramp))
 '(tramp-remote-path
   (quote
    (tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin" tramp-own-remote-path)) nil (tramp))
 '(tramp-verbose 2 nil (tramp)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
