(require 'company)
(require 'jedi)

(setq flycheck-python-pylint-executable "~/anaconda2/bin/pylint")

(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode)
                              (electric-pair-mode)
                              (electric-indent-mode)
                              (jedi-mode)
                              (indent-guide-mode)
                              (modify-syntax-entry ?_ "w")
                              (add-hook 'before-save-hook 'whitespace-cleanup)
                              (add-to-list (make-local-variable 'company-backends)
                                           '(company-jedi company-keywords))
                              ))

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
