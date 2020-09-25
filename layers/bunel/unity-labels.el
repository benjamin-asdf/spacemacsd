;;; Code:
(require 'team-utils)

(defun team-unity/get--labels ()
  (->gg)
  (split-string
   (with-output-to-string
     (when (re-search-forward "labels:" nil t)
       (forward-line 1)
       (while (looking-at "- \\(\\w+$\\)")
         (princ (format "%s\n" (match-string-no-properties 1)))
         (forward-line 1))))))

(defun team-unity/add-labels (file-or-meta -labels)
  (team/with-file
   file-or-meta
   (let* ((curr (team-unity/get--labels))
          (new (-union curr -labels)))
     (unless (= (length new) (length curr))
       (team-unity/set--lables new)))))

(defun team-unity/set--lables (-labels)
  "Assume current buffer has meta syntax, change label part to -LABELS."
  (->gg)
  (when (re-search-forward "labels:" nil t)
    (team/delete-this-line)
    (forward-line 1)
    (while (looking-at "-")
      (team/delete-this-line)
      (forward-line 1))
    (->gg))
  (unless (re-search-forward "guid: " nil t)
    (error "Failed to parse meta %s" (buffer-name (current-buffer))))
  (->$)
  (open-line 1)
  (forward-line 1)
  (insert (team-unity/label-syntax -labels)))

(defun team-unity/label-syntax (-labels)
  "Return the label syntax that unity would put for -LABELS."
  (with-output-to-string
    (princ "labels:")
    (dolist (elm -labels)
      (princ (format "\n- %s" elm)))))

(defun team-unity/get-labels (file-or-meta)
  (team/with-file
   (team-unity/file-or-meta file-or-meta)
   (team-unity/get--labels)))

(provide 'unity-labels)
