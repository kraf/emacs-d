;; Navigating files, buffers and the minibuffer.

;; Distinguish identically-named buffers by their directory instead of <2>.
(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward))

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

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-virtual-abbreviate 'abbreviate
        uniquify-min-dir-content 10)
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package flx
  :after ivy)

(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package amx
  :after ivy
  :config
  (amx-mode 1))

(defun projectile-find-file-other-window-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (find-file-other-window (projectile-completing-read "Find file in projects: " (projectile-all-project-files))))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-create-missing-test-files t)
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
