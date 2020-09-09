;;; Code:
(defun team-create-temp-file-on-region ()
  "Create a file in temp data folder from active region,
return the file name of the create file"
  (interactive)
  (when (region-active-p)
    (let ((file (make-temp-file "team-file" nil nil (buffer-substring (region-beginning) (region-end)))))
      (find-file file)
      file)))


(defun team/find-duplicates (list)
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


;; (defun team/last-eldoc-message-to-reg (&optional register)
;;   "Copy last eldoc message to REGISTER default to register k"
;;   (interactive)
;;   (when team/eldoc-previous-message
;;     (evil-set-register (or register ?k) team/eldoc-previous-message)))

(defun team/last-eldoc-csharp-no-type ()
  (interactive)
  (when team/eldoc-previous-message
    (evil-set-register ?a (string-trim-left team/eldoc-previous-message "\\w+ "))))


(defun team/evil-pop-register ()
  "Pop register 1, move all registers up. So 2 becomes and so on."
  (interactive))




(defalias 'team/push-to-stack-buff (team/build-append-to-buff team/stack-buff t))

(defconst team/stack-buff "*team/stack*")

(defun team/push-to-stack-buff ()
  (interactive)
  (team/build-append-to-buff team/stack-buff))

(defun team/push-region-to-stack ()
  (interactive)
  (team/push-to-stack-buff (region-str)))

(defun team/stack-buff-contents ()
  (team/buff-content
   team/stack-buff))

;; (defun benj-slack/msg-stack-buff ()
;;   (interactive)
;;   (slack-im-select)
;;   (slack-message-send-from-buffer)
;;   (insert team/stack-buff-contents))

(defun team/pop-stack-buff ()
  (interactive)
  "Flush the whole contents of `team/stack-buff' into the current buffer."
  (insert (team/stack-buff-contents))
  (team/erase-that-buff team/stack-buff))





(defmacro team/with-default-dir (dir &rest body)
  "Set `default-directory' to DIR and eval BODY."
  (declare (debug body))
  (declare (indent 2))
  `(let ((default-directory ,dir))
     ,@body))


(defun team/mklist (obj)
  (if (listp obj) obj (list obj)))

(defmacro team/with-file (file &rest body)
  "Goto temp file FILE, insert file contents and evaluate BODY in there.
This also goes to point min point."
  (declare (debug body))
  (declare (indent 2))
  (let ((file-g (gensym)))
    `(let ((,file-g ,file))
       (with-temp-file
            ,file-g
          (insert-file-contents-literally ,file-g)
          (goto-char (point-min))
          ,@body))))

(defmacro team/--with-cs-files (dir &rest forms)
  "Eval FORMS with all cs files. Anaphoric it as the file name."
  `(--map
    (team/with-file it ,@forms)
    (directory-files-recursively ,dir "\\.cs$")))

(defmacro team/regexp-opt (&rest strings)
  `(regexp-opt '(,@strings)))




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




(defvar team/yank-to-letter nil
  "The last used register for `yanks-to-letter-registers'")
(defun team/toggle-yank-to-letter ()
  "Toggle putting yanks into letter registers."
  (interactive)
  (setq  team/yank-to-letter (if team/yank-to-letter nil (- ?a 1)))
  (message
   (if team/yank-to-letter
       "yanks are stored in a,b,c.."
     "not yanking to a,b,c.. anymore")))

(defun team/yank--to-register-adv (func &rest args)
  "Advice for `evil-set-register'."
  (when (characterp team/yank-to-letter)
    (setq team/yank-to-letter
          (team/next-letter-register team/yank-to-letter))
    (evil-set-register team/yank-to-letter (teamel/last-yank))
    (when (> team/yank-to-letter
             (+ ?a 24))
      (setq team/yank-to-letter ?a))))






(advice-add #'evil-yank :after #'team/yank--to-register-adv)

(defun team/special-register-p (character)
  (pcase character
    (?e t)
    (?f t)
    (_ nil)))

(defun team/next-letter-register (it)
  (let ((next (+ 1 it)))
    (or
     (and (team/special-register-p
           next)
          (team/next-letter-register next))
     next)))





(defun team/path-parts (path)
  (--filter (not
             (string-empty-p it))
            (split-string path "/")))


(defun team/relative-path (a-path b-path)
  "The relative path looking from B-PATH to A-PATH, which should be
absolute paths. B-PATH can either be a directory, or a file name."
  (let ((a-parts (team/path-parts a-path))
        (b-parts (team/path-parts (file-name-directory b-path)))
        (res)
        (common-part-index)
        (common-part-over))
    (--map-indexed
     (when (not common-part-over)
       (if (equal (nth-value it-index b-parts) it)
           (setq common-part-index it-index)
         (setq common-part-over t)))
     a-parts)
    (setq res
          (subseq a-parts (+ common-part-index 1)))
    (--dotimes (- (length b-parts) (+ common-part-index 1))
      (push ".." res))
    (mapconcat #'identity res "/")))





(defmacro // (arglist &rest body)
  "Define a lambda with ARGLIST and BODY."
  (declare (indent 2))
  `(lambda ,arglist ,@body))


(defmacro team/when1 (form &rest forms)
  "Eval FORM, and then FORMS, if FORM returns non nil.
Return FORM value like `prog1' and `when' combined."
  `(let ((res ,form))
     (when res
       ,@forms)
     res))

(defun team/nums (until)
  (let ((res))
    (--dotimes until
      (push it res))
    (nreverse res)))


(defmacro team/a-if (test then-form &rest else-forms)
  "If TEST form returns non nil, bind anaphoric it to it, then
eval THEN-FORM and return the return value of THEN-FORM.
Else eval ELSE-FORMS with implicit progn."
  `(let ((it ,test)) (if it ,then-form ,@else-forms)))



;; procs

(defun team/start-proc (name buffer program &rest args)
  "Flatten ARGS and start proc, see `start-process'. If BUFFER is nil, user current buffer."
  (apply #'start-process
         (-flatten
          `(,name
            ,(or buffer (current-buffer))
            ,program
            ,args))))


;; elisp

(defun team/buff-content (buffer-or-name)
  (with-current-buffer
      buffer-or-name
    (buffer-string)))

(defun team/erase-that-buff (buffer-or-name)
  "Erase contents of BUFFER-OR-NAME."
  (with-current-buffer
      (get-buffer-create buffer-or-name)
    (erase-buffer)))

(defun team/build-append-to-buff (buffer-or-name &optional command)
  "Return a closure that takes one argument STRING and
appends STRING into BUFFER-OR-NAME."
  `(lambda (string)
     ,(when command
        '(interactive))
     (with-current-buffer
         (get-buffer-create ,buffer-or-name)
       (team/insert-line string))))

(defun line-> (line)
  "Goto LINE in curr buffer.
This is the version that the manual recommends for going to a line in lisp programs."
  (goto-char (point-min))
  (forward-line (- line 1)))

(defun line->> (line)
  "Goto to the end of LINE. See `line->'"
  `(progn
     (line->
         ,line)
     (goto-char (point-at-eol))))

(defun ->$ (&optional text)
  "Goto the end of the line.
With TEXT, insert TEXT at the end of the line."
  (goto-char (point-at-eol))
  (when text (insert text)))

(defun ->0 ()
  "Goto beginning of line"
  (goto-char (point-at-bol)))

(defun team/re-this-line (reg &optional no-error)
  "Search for REG on current line. If NO-ERROR is non nil,
 do not err, if there is no match"
  (save-excursion
    (->0)
    (re-search-forward reg (point-at-eol) no-error)))

(defun region-str ()
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defmacro team/--each-file (files &rest body)
  (declare (debug body))
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

(defmacro team/while-file-reg (file reg &rest body)
  `(team/with-file
    ,file
    (team/while-reg
     ,reg
     ,@body)))

(defmacro team/re-replace (reg replace)
  `(team/while-reg
    ,reg
    (replace-match ,replace)))


(defmacro team/re-replace-in-file (file reg replace)
  `(team/with-file
    ,file
    (team/re-replace
     reg replace)))


(defun team/insert-line (string)
  (insert string)
  (team/->new-line))

(defun team/in-new-line (string)
  "Insert STRING in new line and indent."
  (line->$)
  (team/->new-line)
  (insert string)
  (when (looking-back "\n")
    (forward-line -1))
  (indent-according-to-mode))

;; the default func for that is really wierd to use
(defun team/re-replace-in-string (string re replace)
  (with-temp-buffer
    (insert string)
    (->gg)
    (team/re-replace re replace)
    (buffer-string)))


(defun team/touch-empty-file (file)
  (write-region "" nil file))


(defun team/collect--reg (reg &optional match)
  "Collect REG matches into a list.
MATCH: The match data group to collect."
  (let ((res '()))
     (team/while-reg
      reg
      (setq res (cons (match-string-no-properties (or match 0)) res)))
     res))

(defun team/collect-reg (file reg match)
  "Collect all REG matcher in FILE.
MATCH: The match data group to collect."
  (team/with-file
   file
   (team/collect--reg reg match)))

(defun line->0 ()
  "Goto beginning of line"
  (goto-char (point-at-bol)))

(defun ->gg ()
  "Go to char min."
  (goto-char (point-min)))

(defun ->G ()
  "Go to char max"
  (goto-char (point-max)))

(defun team/capitalize-first-letter (s)
  (concat (capitalize (substring s 0 1)) (substring s 1)))

(defun team/un-capitalize (s)
  (concat (downcase (subseq s 0 1)) (subseq s 1)))


(defmacro teamel/a-indent (&rest body)
  "Bind current indent to indent and execute body"
  (declare (indent 2))
  `(let ((indent (current-indentation)))
     ,@body))

(defun team/collect-reg-to-buff (file reg match)
  "Collec REG in FILE. Put the output into a buffer and return that buffer."
  (let ((buff (get-buffer-create "collect-reg-buff")))
    (with-current-buffer buff
      (erase-buffer))
    (cl-flet
        ((insert-it
          (it)
          (with-current-buffer
              buff
            (team/insert-line it))))
      (team/while-file-reg
       file
       (insert-it (match-string-no-properties match))))
    buff))

(defun team/insert-indented (&rest strings)
  (team/then-indent-like-here
   (--each
       strings
     (insert it))))

(defmacro team/then-indent-like-here (&rest body)
  "Take the current indent and point, execute body.
Then indent between current point and the old point."
  (declare (debug body))
  `(let ((p (point-marker))
         (indent (current-indentation)))
     ,@body
     (indent-region (min p (point)) (max p (point)) indent)))


(defun team/make-null-term (&optional file)
  "Replace newline for null char in FILE, or the buffer file."
  (interactive)
  (team/re-replace-in-file
   (or file
       (buffer-file-name)
       (user-error "No file provided and buffer is not visiting a file either."))
   "\n"
   "\0"))

(provide 'team-utils)
