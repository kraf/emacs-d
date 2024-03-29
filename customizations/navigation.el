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
