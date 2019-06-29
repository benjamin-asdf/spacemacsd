(defun my-redshift()
  (interactive)
  (let ((command "my_redshift"))
    (async-shell-command command)))


(defun my-open-browser()
  (interactive)
  (let ((command "firefox"))
    (async-shell-command command)))
