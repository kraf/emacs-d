;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (text-scale-decrease 1)))

(setq org-default-notes-file "~/orgs/inbox.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("T" "Todo" entry (file+headline "~/orgs/inbox.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("t" "Todo" entry (file "~/Synced/orgzly/Todo.org")
         "* TODO %?\n %i")
        ("j" "Journal" entry (file+datetree "~/orgs/journal.org")
         "* %?\nEntered on %U\n %i")
        ("p" "People" entry (file+headline "~/orgs/people.org" "Inbox")
         "* %? %T")))

(setq org-agenda-files '("~/Synced/orgzly/Todo.org"))
