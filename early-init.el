;; Performance
(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))

;; Disable package.el so it doesn't fight straight.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)   ;; Emacs 27+
