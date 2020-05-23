(defconst benj-backups-dir "~/.local/.backups/")
(defvar benj-backups-timer '()
  "The timer that is running `benj-backup-some-buffers' default is every 5 min")

(defun benj-backups--file-name (file-name)
  "Get file name for a backup FILE-NAME"
  (concat benj-backups-dir
          (file-name-as-directory (file-name-base (directory-file-name (file-name-directory file-name))))
          (file-name-as-directory (format-time-string "%Y/%m/%d"))
          (format-time-string "%Hh-%Mm-")
          (file-name-nondirectory file-name)))

(defun benj-backup-some-buffers ()
  "Make backup of changed buffers that have files. in `benj-backups-dir'"
  (message "Making best backups..")
  (unless (file-exists-p benj-backups-dir)
    (make-directory benj-backups-dir t))
  (--map-when
   (and
    (buffer-modified-p it)
    (buffer-file-name it)
    (file-exists-p
     (buffer-file-name it)))
   (let ((file-name (benj-backups--file-name (buffer-file-name it))))
     (unless (file-exists-p (file-name-directory file-name))
       (make-directory (file-name-directory file-name) t))
     (with-temp-file
         file-name
       (insert-buffer it)
       (message "wrote backup in %s" file-name)))
   (buffer-list)))
(setq benj-backups-timer (run-at-time (* 5 60) (* 5 60) 'benj-backup-some-buffers))
