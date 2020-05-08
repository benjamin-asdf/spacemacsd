




















(defun team-create-temp-file-on-region ()
  "Create a file in temp data folder from active region"
  (interactive)
  (let ((file (make-temp-file "team-file" nil nil (buffer-substring (region-beginning) (region-end)))))
    (find-file file)))
