;; -*- lexical-binding: t; -*-

(define-minor-mode benj-python-electric-bools-mode 
  "Minor mode for electrictly fixing bool syntax in python."
  :lighter " ebool"
  :group 'electric
  (if benj-python-electric-bools-mode
      (add-hook 'post-self-insert-hook #'benj-python-electric-bools-post-self-insert-fn 0 t)
    (remove-hook
     'post-self-insert-hook #'benj-python-electric-bools-post-self-insert-fn t)))


(let ((t-re (regexp-opt (list "true" " t " "ture")))
      (f-re (regexp-opt (list "false" " f " "fasle")))
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

(put 'python-pylint (flycheck--checker-property-name 'error-filter) #'benj-pylint-checker-filter)

(defun benj-pylint-checker-filter (errors)
  (flycheck-sanitize-errors
  (--remove
    (string-match-p
     "Instance of.+?has no.+?member; maybe '_"
     (flycheck-error-message it))
    errors)))

(provide 'python-config)

