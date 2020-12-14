;; todo utils
(defun team/listify-cons (it)
  "Make a list out of a cons cell."
  (unless
      (listp (cdr it))
    (setf
     (cdr it)
     (cons (cdr it) nil)))
  it)


(defun my/c-beginning-of-class ()
  "Move to the beginning of the class around point.
If unsucessful, move to point min and return nil."
  (interactive)
  (while
      (and
       (c-beginning-of-defun)
       (not (looking-at "^.*\\bclass\\b.*$")))))


(defun my/c-end-of-class ()
  "Try move to the end of class around point."
  (interactive)
  (my/c-beginning-of-class)
  (skip-chars-forward "^{")
  (forward-char -1)
  (forward-sexp))

(defun my/csharp-kill-current-class-name ()
  "Try to copy the current csharp class name as kill."
  (interactive)
  (save-excursion
    (my/c-beginning-of-class)
    (if
        (re-search-forward
         ".*class[[:blank:]]\\(\\w+\\)"
         (point-at-eol)
         t
         1)
        (-->
         (match-string-no-properties 1)
         (kill-new it)
         (message "Copied \"%s\" as kill." it))
      (user-error "Did not find csharp class here."))))



(defun my/csharp-delete-block ()
  (-some-->
      (my/csharp-block-bounds)
    (delete-region
     (car it)
     (cdr it))))

(defun my/csharp-block-bounds ()
  "Return a cons cell consisting of beginning and end markers of the current cshrap block.
Return nil, if not inside a block."
  (-some-->
      (progn
        (goto-char (point-at-eol))
        (skip-chars-backward "^{")
        (forward-char -1)
        (when
            (c-backward-to-block-anchor)
          (point-marker)))
    (cons
     it
     (progn
       (skip-chars-forward
        "^{")
       (c-forward-sexp)
       (point-marker)))))

(defun my/chsarp-raise-block ()
  " Try to raise the current csharp block COUNT time.
Similar to lisp structural rais sexp."
  (interactive)
  (when-let* ((inner-bounds
               (my/csharp-block-bounds))
              (s
               (progn
                 (print inner-bounds)
                 (buffer-substring
                  (car inner-bounds)
                  (cdr inner-bounds)))))
    (delete-region
     (car inner-bounds)
     (cdr inner-bounds))
    (my/csharp-delete-block)
    (let ((p (point-marker)))
      (insert s)
      (indent-region-line-by-line
       p
       (point)))))


(defun my/csharp-convert-foreach-to-for ()
  "Convert foreach syntax on this line to a for loop."
  (interactive)
  (when
      (team/re-this-line
       "foreach.+?var[[:blank:]]+\\(\\w+\\)[[:blank:]]+in[[:blank:]]+\\(\\w+\\)")
    (let ((var-name (match-string-no-properties 1))
          (list-name (match-string-no-properties 2))
          (bounds
           (my/csharp-block-bounds)))
      (goto-char
       (car bounds))
      (let ((p (point-marker))
            (content
             (buffer-substring
              (progn
                (skip-chars-forward "^{")
                (forward-char 1)
                (point))
              (cdr bounds))))
        (apply
         #'delete-region
         (team/listify-cons
          bounds))
        (insert (format
                 "for (var i = 0; i < %s; i++) {"
                 (concat
                  list-name
                  ".Count")))
        (team/in-new-line
         (format
          "var %s = %s[i];"
          var-name
          list-name))
        (insert
         content)))))

(provide 'c-moves)
