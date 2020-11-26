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



(provide 'c-moves)
