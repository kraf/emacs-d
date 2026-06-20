;; Navigating files, buffers and the minibuffer.
;; Minibuffer completion: vertico + orderless + marginalia + consult + embark.

;; Distinguish identically-named buffers by their directory instead of <2>.
(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-min-dir-content 10))

(use-package recentf
  :straight nil
  :custom
  (recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-max-menu-items 40)
  :config
  (recentf-mode 1))

(setq enable-recursive-minibuffers t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package which-key
  :straight nil ; built-in since Emacs 30
  :config
  (which-key-mode 1))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  :config
  (vertico-mode 1)
  ;; Live layout toggles in the minibuffer (from the vertico package):
  ;;   M-B buffer (large pop-up)  M-G grid  M-V vertical  M-F flat  M-R reverse
  (require 'vertico-multiform)
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (side . right)
          (window-width . 0.5)))
  (vertico-multiform-mode 1))

;; Persist minibuffer history; vertico sorts by it (replaces amx).
(use-package savehist
  :straight nil
  :config
  (savehist-mode 1))

;; Space-separated, order-free matching everywhere.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; Each space-separated component matches as a literal/regexp substring OR a
  ;; flex (fuzzy) match, so "fb" finds "foo-bar".
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Annotate candidates (docstrings, file sizes, keybindings, ...).
(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-c k" . consult-ripgrep))
  :custom
  (consult-narrow-key "<")
  :init
  ;; Route xref through consult, so lsp-find-references / lsp-find-definition
  ;; and plain xref present their hits in the vertico minibuffer (with preview).
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Commands that preview by opening another file are slow to scroll, so
  ;; debounce their preview: it stays automatic but only fires after a short
  ;; pause, so fast scrolling never lags. M-. still previews on demand.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-recent-file consult-xref
   ;; consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.6 any "M-.")))

;; Act on the thing at point / the current candidate.
(use-package embark
  :bind (("C-," . embark-act)
         ("C-h B" . embark-bindings)))

;; embark-export from consult-ripgrep gives an editable grep buffer (wgrep).
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; wgrep has no autoloads; load it when the first grep buffer appears.
(use-package wgrep
  :hook (grep-setup . wgrep-setup))

(defun projectile-find-file-other-window-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (find-file-other-window (projectile-completing-read "Find file in projects: " (projectile-all-project-files))))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-create-missing-test-files t)
  (projectile-completion-system 'default)
  :config
  (projectile-mode 1)
  (define-key projectile-command-map (kbd "4 F") #'projectile-find-file-other-window-in-known-projects))

(use-package treemacs
  :defer t)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package ace-window
  :bind (("M-w" . ace-window)
         ("C-x o" . ace-window)))
