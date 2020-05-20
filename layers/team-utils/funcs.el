














(defun team-create-temp-file-on-region ()
  "Create a file in temp data folder from active region,
return the file name of the create file"
  (interactive)
  (when (region-active-p)
    (let ((file (make-temp-file "team-file" nil nil (buffer-substring (region-beginning) (region-end)))))
      (find-file file)
      file)))


(defun stack-overlow-find-duplicates (list)
  "Return a list that contains each element from LIST that occurs more than once."
  (--> list
       (-group-by #'identity it)
       (-filter (lambda (ele) (> (length ele) 2)) it)
       (mapcar #'car it)))


(defun team-make-backup-file ()
  "Make backup file of currently visited file."
  (interactive)
  (when (buffer-file-name)
    (copy-file (buffer-file-name) (concat (buffer-file-name ".bak")) t)))

(defun team-rename-thing-at-point ()
  "Prompt to rename the word-to-replace at point. In buffer."
  (interactive)
  (let* ((word-to-replace (thing-at-point 'word))
         (replace-with (read-from-minibuffer "Rename to: " word-to-replace)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward word-to-replace nil t)
        (replace-match replace-with t)))))


;; TODO use (perf)
;;(directory-files-and-attributes )
(defun latest-file (path)
  "Get latest file (including directory) in PATH."
  (car (sort (--remove (member (file-name-nondirectory it) '(".." ".")) (directory-files path 'full nil t)) #'file-newer-than-file-p)))
