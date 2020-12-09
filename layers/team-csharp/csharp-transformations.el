

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
      (c-beginning-of-defun)
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


(defun team-csharp/wrap-if (&optional cond-part)
  (interactive)
  (forward-line 0)
  (kill-line 1)
  (insert
   (format
    "if (%s) {
%s}
"
    (or cond-part "")
    (teamel/last-yank)))
  (indent-region-line-by-line
   (save-excursion (forward-line -3) (point))
   (point))
  (unless
      cond-part
    (re-search-backward
     "if ()")
    (forward-char 4)
    (evil-insert-state)))



(defun team-csharp/turn-conditional-to-log ()
  "Assume region string in a csharp conditional, copy log string as kill."
  (interactive)
  (kill-new
   (let ((s (region-str)))
     (with-temp-buffer
       (insert s)
       (->gg)
       (when (re-search-forward
              "if.+?(\\(.*\\))" nil t)
         (replace-match "\\1"))
       (while
           (re-search-forward
            (regexp-opt
             (list
              "return"
              ";"))
            nil
            t)
         (replace-match ""))
       (let ((elms
              (-map
               #'s-trim
               (s-split
                (regexp-opt
                 (list
                  "&&"
                  "||"))
                (buffer-string)))))
         (erase-buffer)
         (insert
          "Debug.Log($\"")
         (--each elms
           (insert
            it)
           (insert " : ")
           (insert "{")
           (insert it)
           (insert "}")
           (insert ",\\n"))
         (insert "\");"))
       (buffer-string)))))


(provide 'csharp-transformations)
