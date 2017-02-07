(setq company-dabbrev-downcase nil)
(setq company-backends
      (quote
       (company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-files company-capf
                     (company-dabbrev-code company-gtags company-etags company-keywords)
                     company-oddmuse company-dabbrev)))
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)
