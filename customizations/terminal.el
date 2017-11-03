(defun silence ()
  (interactive))

(if (not (display-graphic-p))
    (progn
      (xterm-mouse-mode)
      (set-face-background 'region "#81660a")
      (define-key evil-motion-state-map [down-mouse-1] nil)
      (define-key evil-motion-state-map [mouse-1] nil)
))
