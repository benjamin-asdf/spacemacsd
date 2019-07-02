(defun my-redshift()
  (interactive)
  (let ((command "fish -c my_redshift"))
    (shell-command command)))


(defun my-open-browser()
  (interactive)
  (let ((command "firefox"))
    (async-shell-command command)))
