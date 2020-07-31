(defconst benj-backups-dir "~/.local/.backups/")
(defvar benj-backups-timer '()
  "The timer that is running `benj-backup-some-buffers' default is every 5 min")

(defun benj-backups--file-name (file-name)
  "Get file name for a backup FILE-NAME"
  (concat benj-backups-dir
          (file-name-as-directory
           (file-name-base (directory-file-name (or (file-name-directory file-name) "scratches"))))
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
   (benj-backups/make-backup)
   (buffer-list)))

(defun benj-backup/make-curr-buff-backup ()
  "Make backup of current buffer. Respects scratch buffers."
  (interactive)
  (benj-backups/make-backup
   (current-buffer)
   (when (string-match-p
          "*scratch"
          (buffer-name))
     (benj-backups--file-name (string-trim (buffer-name) "*" "*")))))

(defun benj-backups/make-backup (buff &optional file-name)
  (when (not (or file-name (buffer-file-name buff)))
    (user-error "buffer is not visiting a file. And not handled specially."))
  (let ((file-name (or file-name (benj-backups--file-name (buffer-file-name buff)))))
    (unless (file-exists-p (file-name-directory file-name))
      (make-directory (file-name-directory file-name) t))
    (with-temp-file
        file-name
      (insert-buffer buff)
      (message "wrote backup in %s" file-name))))

(setq benj-backups-timer (run-at-time (* 5 60) (* 5 60) 'benj-backup-some-buffers))
