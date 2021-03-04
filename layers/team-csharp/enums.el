
(define-error 'enum-parse-failure "Failed parsing enum.")

(defun team-chsarp/expand-syntax (&optional end)
  ""
  (unless end
    (setf end
          (save-excursion
            (skip-chars-forward "^}")
            (point))))
  (save-excursion
    (while
        (re-search-forward
         (regexp-opt
          (list "{" "}" ","))
         end t)
      (open-line 1))))


(defmacro team-csharp/with-sanitized-region (start end &rest body)
  "Put the buffer substring between START and END into a temp buffer, call `team-csharp/exand-syntax', remove chsarp comment syntax, then eval BODY."
  (declare (debug body))
  `(let ((s
         (buffer-substring
          ,start
          ,end)))
    (with-temp-buffer
      (insert s)
      (->gg)
      (while
          (re-search-forward
           "//"
           nil t)
        (delete-region
         (- (point) 2)
         (point-at-eol)))
      (->gg)
      ,@body)))

(defun team-csharp/enum-values ()
  "Catch enum values around point.
This evaluates to a list of lists. Each element is of the form
(NAME NUM). NUM is optional if there is a \" = digit\" part in the definition of the enum."
  (unless
      (progn
        (goto-char (point-at-bol))
        (and
         (looking-at ".*enum")
         (skip-chars-forward "^{")))
    (signal 'enum-parse-failure "No enum here"))
  (forward-line 1)
  (team-csharp/with-sanitized-region
   (point)
   (save-excursion
     (skip-chars-forward
      "^}")
     (point))
   (let ((res '())
         (last-elm-pos))
     (while
         (re-search-forward
          "\\([[:blank:]]\\(\\w+\\)\\(?: = \\([[:digit:]]+\\)\\)?\\)"
          (save-excursion
            (skip-chars-forward
             "^}")
            (point))
          t)
       (when (match-string-no-properties 2)
         (setq last-elm-pos (point))
         (push (list (match-string-no-properties 2)
                     (match-string-no-properties 3))
               res)))
     (goto-char last-elm-pos)
     (nreverse res))))


(defun team-csharp/count-enum-value ()
  "Read from the enum values on point.
Say enum value number."
  (interactive)
  (when
      (re-search-backward "\\benum\\b")
    (let* ((vals (team-csharp/enum-values))
           (choice
            (completing-read "value for num: "
                             (al-keys vals))))
      (message
       "%s is number %d in this enum."
       choice
       (cl-loop
        for it in vals
        with it-index = -1
        do (incf it-index)
        when (and
              (string-equal
               choice
               (car it))
              it)
        do (return (or (cadr it) it-index)))))))




(defun team-csharp/append-to-enum (name &optional num-syntax)
  "Append NAME to the botton of enum around point. When NUM-SYNTAX is non nil,
also add a number, which is either the +1 the last of such numbers, or +1 the count of enum elements."
  (team/a-when
   (team-csharp/enum-values)
   (team/in-new-line
    (format "%s%s," name
            (if num-syntax
                (format " = %d"
                        (+ 1
                           (or (string-to-number
                                (cadar (last it)))
                               (length it))))
              "")))))



(defun benj/simple-csharp-enum-values ()
  "Return a list of strings of the enum at point."
  (-remove
   #'string-empty-p
   (-map
    #'s-trim
    (s-split ","
             (buffer-substring
              (progn (forward-line) (point-at-bol))
              (save-excursion (skip-chars-forward "^}") (point)))
             t))))

(provide 'enums)
