(server-start)

(setq display-time-default-load-average nil)
(display-time-mode t)
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
                    'face 'egoge-display-time)))

(require 'exwm)
(require 'exwm-config)

(exwm-config-ido)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 10)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,(- i 1)))))
                  (number-sequence 1 9))
        
        ;; Bind "s-0 0" to "s-0 9" to 10 - 11
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-0 %d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,(+ i 9)))))
                  (number-sequence 0 9))
        
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-o] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; Bind "s-l" to "slock", a simple X display locker.
        ([?\s-l] . (lambda ()
                     (interactive)
                     (start-process "" nil "/usr/bin/slock")))))

;; Set global EXWM key bindings
(exwm-input-set-key (kbd "s-t") #'exwm-floating-toggle-floating)
(exwm-input-set-key (kbd "s-s") #'exwm-workspace-switch-to-buffer)
(exwm-input-set-key (kbd "s-m") #'exwm-workspace-move-window)
(exwm-input-set-key (kbd "s-z") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "s-<f2>") #'smex)
(exwm-input-set-key (kbd "s-<return>") (lambda () 
                                         (interactive)
                                         (start-process-shell-command "Terminal" nil "gnome-terminal")))

(exwm-input-set-key (kbd "s-v") (lambda () 
                                  (interactive)
                                  (select-window (split-window-vertically))
                                  (balance-windows)))

(exwm-input-set-key (kbd "s-h") (lambda () 
                                  (interactive)
                                  (select-window (split-window-horizontally))
                                  (balance-windows)))

(exwm-input-set-key (kbd "s-q") #'delete-window)
(exwm-input-set-key (kbd "s-o") (lambda (command)
                                  (interactive (list (read-shell-command "$ ")))
                                  (start-process-shell-command command nil command)))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'desktop-environment-brightness-increment)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'desktop-environment-brightness-decrement)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment)
(exwm-input-set-key (kbd "s-<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment-slowly)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement)
(exwm-input-set-key (kbd "s-<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement-slowly)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'desktop-environment-toggle-mute)

;; FIXME why doesnt this work?
;; (exwm-input-set-key (kbd "s-<f4>") (lambda ()
;;                                      (interactive)
;;                                      (kill-current-buffer)))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
;; (define-key exwm-mode-map [?\s-t] #'exwm-input-toggle-keyboard)
;; (define-key exwm-mode-map [s-Q] #'kill-this-buffer) ;; doesnt work

(setq exwm-manage-configurations '((t char-mode t)))

;; don't copy on selct
(setq select-enable-primary nil)

(display-battery-mode t)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(exwm-enable)

;; TODO
;; - how quit via keyboard?
;; - how setup default workspaces with running commands
