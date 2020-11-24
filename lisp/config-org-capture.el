(defun benj/org-capture-person-template (key name)
  "Return an entry for `org-capture-templates', which see.
KEY, NAME: see `org-capture-templates'."
  `(,key ,(format "Todo item %s" name) entry
         (file ,(format "~/org/%s.org" name))
         "* TODO %? :newitem:\n %i\n %a"))


(defun benj/org-remove-tag-in-buff (&optional tag)
  (interactive)
  (let*  ((current-tags
		       (cl-remove-if (lambda (tag) (get-text-property 0 'inherited tag))
			                   (org-get-tags)))
          (tag
           (or
            tag
            (org-trim (completing-read
				               "Tags: "
				               #'org-tags-completion-function
				               nil nil (org-make-tag-string current-tags)
				               'org-tags-history)))))
    (save-excursion
      (->gg)
      (while
          (re-search-forward
           (concat ":?" tag ":?") nil t)
        (replace-match "")))))

(defun benj/finish-org-file-day ()
  "Remove all new items and add a ^L at the bottom of current buffer."
  (interactive)
  (benj/org-remove-tag-in-buff "newitem")
  (save-excursion
    (goto-char (point-max))
    (open-line 1)
    (forward-line 1)
    (insert-char 12)
    (open-line 1)))



(setq
 org-capture-templates
 (list
  (benj/org-capture-person-template "r" "rico")
  (benj/org-capture-person-template "j" "jan")
  '("b" "benj item" entry
    (file "~/org/notes.org"))))


(spacemacs/declare-prefix-for-mode
  'org-mode
  "ot"
  "tags")

(spacemacs/set-leader-keys-for-major-mode
  'org-mode
  "otr"
  #'benj/org-remove-tag-in-buff)

(spacemacs/set-leader-keys-for-major-mode
  'org-mode
  "ol"
  #'benj/finish-org-file-day)



(provide 'config-org-capture)
