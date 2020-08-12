














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


;; TODO use (directory-files-and-attributes )
(defun latest-file (path)
  "Get latest file (excluding directory) in PATH."
  (car (sort (--remove (member (file-name-nondirectory it) '(".." ".")) (directory-files path 'full nil t)) #'file-newer-than-file-p)))




(defvar team/eldoc-previous-message "")
(defun team/eldoc-save-last-message (orig &rest args)
  "Meant as an advice for `eldoc-message'."
  ;; override register e always with last msg
  (evil-set-register
   ?e
   (setq team/eldoc-previous-message
         ;; when eldoc clears the buffer this will be the empty string.
         ;; only keep track of the actual value
         (or (apply orig args) team/eldoc-previous-message))))
(advice-add 'eldoc-message :around #'team/eldoc-save-last-message)


(defun team/last-eldoc-message-to-reg (&optional register)
  "Copy last eldoc message to REGISTER default to register k"
  (interactive)
  (when team/eldoc-previous-message
    (evil-set-register (or register ?k) team/eldoc-previous-message)))


(defun team/evil-pop-register ()
  "Pop register 1, move all registers up. So 2 becomes and so on."
  (interactive))


(defmacro team/with-file (file &rest body)
  "Goto temp file FILE, insert file contents and evaluate BODY in there.
This also goes to point min point."
  (declare (debug body))
  (declare (indent 2))
  `(with-temp-file
      ,file
    (insert-file-contents-literally ,file)
    (goto-char (point-min))
    ,@body))





(defun team/regex-builder-with-region ()
  "Copy region into a temp file and start regex builder there"
  (interactive)
  (let ((str (region-str)))
    (find-file (team-create-temp-file-on-region))
    (delete-other-windows)
    (regexp-builder)
    (with-current-buffer
        (get-buffer reb-buffer)
      (insert str))))





(defmacro // (arglist &rest body)
  "Define a lambda with ARGLIST and BODY."
  `(lambda ,arglist ,@body))


;; TODO move into some package "smoves.el" "moo.el" ?
(defmacro line-> (line)
  "Goto LINE in curr buffer.
This is the version that the manual recommends for going to a line in lisp programs."
  (declare (indent 1))
  `(progn
     (goto-char (point-min))
     (forward-line (- ,line 1))))

(defmacro line->> (line)
  "Goto to the end of LINE. See `line->'"
  `(progn
     (line->
         ,line)
     (goto-char (point-at-eol))))

(defun region-str ()
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defmacro team/--each-file (files &rest body)
  `(--map
    (team/with-file
     it
     ,@body)
    ,files))

(defmacro team/while-reg (reg &rest body)
  `(while (re-search-forward
           ,reg
           nil t)
     ,@body))


(defmacro team/re-replace (reg replace)
  `(team/while-reg
    ,reg
    (replace-match ,replace)))

(defun ->$ (&optional text)
  "Goto the end of the line.
With TEXT, insert TEXT at the end of the line."
  (goto-char (point-at-eol))
  (when text (insert text)))


(defun team/touch-empty-file (file)
  (write-region "" nil file))
