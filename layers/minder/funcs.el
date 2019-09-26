(defconst minder-meme-journal-dir "~/.local/minder/memetic-journal")

(defun minder--todays-journal-file ()
  "Get the push message file for the current day."
  (concat (file-name-as-directory minder-meme-journal-dir) (format-time-string "%F")))

(defun minder-push-message (msg)
  "Open the push messages file and push MSG."
  (interactive "sWhat is the current meme?")
  (minder--push-message msg))

(defun minder--push-message (msg)
  "Open the messages file and push MSG."
  (let ((curr-buffer (buffer-name)))
    (find-file-other-window (minder--todays-journal-file))
    (goto-char (point-max))
    (insert (format "[%s] %s\n" (format-time-string "%T") msg))
    (save-buffer)
    (switch-to-buffer-other-window curr-buffer)))

(defun minder-push-best-message ()
  "Push one of messages defined in .config/my-messages to minder file."
  (interactive)
  (minder--push-message (benj-best-message)))


;; automatically save it and push it to a repo maybe
