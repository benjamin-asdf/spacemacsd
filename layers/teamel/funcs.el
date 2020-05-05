


(defun teamel-view-log ()
  "Usage: Click view complete raw in the job log, copy url into clipboard.
Expects some valid url in the unamed register"
  (interactive)
  (let ((buff-name "logs"))
    (pop-to-buffer buff-name)
    (erase-buffer)
    (follow-mode)
    (start-process "get-logs" buff-name "curl" (evil-get-register ?\"))))
