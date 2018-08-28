(setq company-dabbrev-downcase nil)
;; (setq company-backends
;;       (quote
;;        (company-nxml company-css company-eclim company-semantic company-clang company-cmake company-files company-capf
;;                      (company-dabbrev-code company-keywords)
;;                      company-oddmuse company-dabbrev)))
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet
         company-abbrev
         company-dabbrev)))

(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)
