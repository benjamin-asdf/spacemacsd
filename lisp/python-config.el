;; -*- lexical-binding: t; -*-

(define-minor-mode benj-python-electric-bools-mode 
  "Minor mode for electrictly fixing bool syntax in python."
  :lighter " ebool"
  :group 'electric
  (if benj-python-electric-bools-mode
      (add-hook 'post-self-insert-hook #'benj-python-electric-bools-post-self-insert-fn 0 t)
    (remove-hook
     'post-self-insert-hook #'benj-python-electric-bools-post-self-insert-fn t)))


(let ((t-re (regexp-opt (list "true" "t " "ture")))
      (f-re (regexp-opt (list "false" "f " "fasle")))
      (n-re (regexp-opt (list "nil" "non " "none"))))
  (defun benj-python-electric-bools-post-self-insert-fn ()
    "Function to run in `post-self-insert-hook' in python mode to fix some bools."
    (when benj-python-electric-bools-mode
      (cond
       ((looking-back t-re) (replace-match "True"))
       ((looking-back f-re) (replace-match "False"))
       ((looking-back n-re) (replace-match "None"))))))

(add-hook
 'python-mode-hook
 #'benj-python-electric-bools-mode)

(provide 'python-config)
