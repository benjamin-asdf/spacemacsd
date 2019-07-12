(defun my-open-browser()
  (interactive)
  (let ((command "firefox"))
    (async-shell-command command)))
