

(with-eval-after-load 'org

  (defconst benj-trello-directory (concat (file-name-as-directory org-directory) "trello"))
  (defconst benj-trello-ben-board-file (concat (file-name-as-directory benj-trello-directory) "ben.org"))


  (defun benj-trello-pull-board-async ()
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
      (message "Finished syncing trello file.")))




  ;; TODO get the 'short url'
  (defun benj-trello-ghetto-get-card-url ()
    "Evaluate to a linkable uri of the trello card at point.
See `orgtrello-controller-jump-to-card'
"(let* ((full-meta       (orgtrello-buffer-entry-get-full-metadata))
        (entity          (orgtrello-data-current full-meta))
        (right-entity-fn (cond ((orgtrello-data-entity-item-p entity)
                                'orgtrello-data-grandparent)
                               ((orgtrello-data-entity-checklist-p entity)
                                'orgtrello-data-parent)
                               ((orgtrello-data-entity-card-p entity)
                                'orgtrello-data-current))))
   (-when-let (card-id (->> full-meta
                            (funcall right-entity-fn)
                            orgtrello-data-entity-id))
     (orgtrello-setup-compute-url (format "/c/%s" card-id)))))


  (defun benj-trello-copy-card-as-yank ()
    "See `benj-trello-ghetto-get-card-url' copy url as kill."
    (interactive)
    (kill-new (benj-trello-ghetto-get-card-url))))


;; (async-start
;;  (lambda ()
;;    (sleep-for 3)
;;    30)
;;  (lambda (res) (message "done %s" res)))

;; TODO something to set a currently doing task, then keybingins for moving to done or visit url
;; prob some org will do that nicely




;; (run-at-time )


;; TODO put an advice for this func, it did something shit with ido, just want helm
;; (defun orgtrello-controller-choose-board (boards)
;;   "Given a BOARDS map, ask the user to choose from.
;; This returns the identifier of such board."
;;   (-> (completing-read
;;        "Board to install, you mofo: "
;;        (orgtrello-hash-keys boards))
;;       (gethash boards)))
