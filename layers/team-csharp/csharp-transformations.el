

(defun team/chsarp-params-transform ()
  "Dwim transform buffer contents into chsarp parameter syntax."
  (->gg)
  (while (> (point-max) (point))
    (forward-char 1)
    (cond
     ((looking-at ";") (replace-match ","))
     ((looking-back "\n") (replace-match " "))))
  (insert (string-trim
           (prog1
               (buffer-string)
             (erase-buffer)) nil ", ")))

(defun team/insert-yank-as-param ()
  (interactive)
  (with-temp-buffer
    (yank)
    (team/chsarp-params-transform)
    (buffer-string)))

(defun team/csharp-eldoc-to-param ()
  "Take the last omnisharp eldoc message, try to be dwim about what to
add to the paramer list of the enclosing function."
  (interactive)
  (-some-->
      team/eldoc-previous-message
    (with-temp-buffer
      (insert it)
      (->gg)
      (when (re-search-forward "(\\(.*\\))" nil t)
        (insert (prog1 (match-string-no-properties 1) (erase-buffer))))
      (buffer-string))
    (save-excursion
      (csharp-move-back-to-beginning-of-defun)
      (team/^$-replace
       "(\\(.*\\))"
       (let ((part (match-string 1)))
         (format
          "(%s%s%s)"
          part
          (or (and (string-empty-p part) part) ", ")
          it))))))

(defun team/csharp-eldoc-expand-args ()
  "Use `team/eldoc-previous-message' to expand args inside the method call on line."
  (interactive)
  (-some-->
      team/eldoc-previous-message
    (with-temp-buffer
      (insert it)
      (team/^$-replace ".*\\((.*)\\).*" "\\1")
      (buffer-string))
    (team/^$-replace "(.*)" it)))

(defun csharp-delete-curly-body ()
  (interactive)
  (skip-chars-forward "^{")
  (delete-region
   (save-excursion
     (forward-char 1)
     (point))
   (progn
     (forward-list)
     (forward-char -1)
     (point))))



(provide 'csharp-transformations)
