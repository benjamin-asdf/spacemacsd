(defun my-redshift()
  (interactive)
  (let ((command "my_redshift"))
    (async-shell-command command)))
