;; Terminal emulator and TTY tweaks.

(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :defer t)

;; Running in a terminal frame
(unless (display-graphic-p)
  (set-face-background 'region "#81660a")
  (define-key evil-motion-state-map [down-mouse-1] nil)
  (define-key evil-motion-state-map [mouse-1] nil))
