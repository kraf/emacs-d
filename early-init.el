;; Performance
(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))

;; Some packages require package.el for its APIs even when straight.el owns
;; package management. Keep package.el away from the old ELPA tree so straight
;; doesn't warn about duplicate package installations.
(setq package-user-dir (expand-file-name ".disabled-elpa" user-emacs-directory))

;; Disable package.el so it doesn't fight straight.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)   ;; Emacs 27+
