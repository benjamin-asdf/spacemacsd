
(defun benj/org-capture-person-template (key name)
  "Return an entry for `org-capture-templates', which see.
KEY, NAME: see `org-capture-templates'."
  `(,key ,(format "Todo item %s" name) entry
         (file ,(format "~/org/%s.org" name))
         "* TODO %?\n %i\n %a"))

(setq
 org-capture-templates
 (list
  (benj/org-capture-person-template "r" "rico")
  (benj/org-capture-person-template "j" "jan")
  '("b" "benj item" entry
    (file "~/org/notes.org"))))




(provide 'config-org-capture)
