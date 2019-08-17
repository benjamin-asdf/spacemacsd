(defun benj-external-browser()
  (interactive)
  (let ((command "firefox"))
    (async-shell-command command)))
