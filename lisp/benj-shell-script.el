

(defface benj/shell-args-face
  '((t :bold t :height 1.1 :foreground "orange"))
  "Face for shell script args.")

(defun benj/shell-add-args-fontlock-keywords ()
  (font-lock-add-keywords
   nil
   '(("\\(-?-\\w+\\)?-?-\\w+\\b" . 'benj/shell-args-face)) t))

(add-hook 'sh-mode-hook #'benj/shell-add-args-fontlock-keywords)


(provide 'benj-shell-script)
