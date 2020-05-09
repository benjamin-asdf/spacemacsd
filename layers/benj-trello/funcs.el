











(defconst benj-trello-directory (concat (file-name-as-directory org-directory) "trello"))

(defconst benj-trello-ben-board-file (concat (file-name-as-directory benj-trello-directory) "ben.org"))


(defun benj-async-pull-trello ()
  "Pull ben trello boad async.
`benj-trello-ben-board-file' needs to be an initialzied org-trello board file"
  (interactive)
  (copy-file benj-trello-ben-board-file (concat benj-trello-ben-board-file ".old") t)
  (async-start
   (lambda ()
     (with-temp-file benj-trello-ben-board-file
       (insert-file-contents-literally benj-trello-ben-board-file)
         (org-trello-sync-buffer))))
  (lambda (res)
    (message "Finished syncing trello file.")
    )
  )




;; (run-at-time )
