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
    (error "Not on an enum"))
  (forward-line 1)
  (let ((res '())
        (last-elm-pos))
    (while
        (re-search-forward
         "\\(?:^.*//.*$\\)\\|\\([[:blank:]]\\(\\w+\\)\\(?: = \\([[:digit:]]+\\)\\)?\\)"
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
    (nreverse res)))




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
