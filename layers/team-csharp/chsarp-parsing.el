

(defun team/read-chsarp-map (str)
  (let ((res))
    (with-temp-buffer
      (insert str)
      (->gg)
      (while
          (re-search-forward "(\\(.+?\\),.+?\\(.+?\\))" nil t)
        (push `((,(match-string 1) ,(match-string 2))) res)))
    res))


(defun team-csharp-parse-arg-list (s)
  "Return s as csharp arglist.
Splitting by \",\" on its own does not do it, since you can have nested arglists."
  (let ((arglist))
    (with-temp-buffer
      (insert s)
      (->gg)
      (while (not (eobp))
        (push
         (string-trim
          (string-trim-left
           (apply
            #'buffer-substring
            (list
             (point)
             (progn
               (forward-char 1)
               (goto-char
                (min
                 (save-excursion
                   (skip-chars-forward "^,")
                   (point))
                 (save-excursion
                   (skip-chars-forward "^(")
                   (point))))
               (when (eq (char-after) ?\()
                 (forward-list)
                 (skip-chars-forward "^,")
                 )
               (point))))
           ","))
         arglist)))
    (nreverse arglist)))

(provide 'csharp-parsing)
