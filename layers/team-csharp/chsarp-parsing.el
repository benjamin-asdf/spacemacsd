

(defun team/read-chsarp-map (str)
  (let ((res))
    (with-temp-buffer
      (insert str)
      (->gg)
      (while
          (re-search-forward "(\\(.+?\\),.+?\\(.+?\\))" nil t)
        (push `((,(match-string 1) ,(match-string 2))) res)))
    res))

(provide 'csharp-parsing)
