;; youtube-dl
;; adb pull
;; and push
(defconst benj-phone-misc-dir "/storage/self/primary/misc-downloads")

(defun benj-phone-push (source dest)
  "Push SOURCE file into DEST.
SOURCE is a file on the disk, DEST is an absolute on the phone."
  (benj-phone-abd-cmd "push" source dest))

(defun benj-phone-abd-cmd (&rest args)
  "Run adb with ARGS and pop to puffer."
  (pop-to-buffer (process-buffer (benj-start-proccess-flatten-args "abd" "*abd*" "adb" "-d" args))))

(defun benj-phone-push-to-misc-dir (&optional file)
  "Push FILE to `benj-phone-misc-dir'"
  (interactive)
  (team/with-default-dir
   "~/Videos/"
   (call-interactively
    (lambda (f)
      (interactive "fFile to push: ")
      (benj-phone-push f benj-phone-misc-dir)) file)))


(defun benj/youtube-dl-download-yank ()
  "Download current yank into ~/Vidoes."
  (interactive)
  (let ((default-directory "~/Videos"))
    (async-shell-command (format "youtube-dl --no-playlist \"%s\"" (evil-get-register ?\")))))

(defun benj/youtube-dl-download-and-push ()
  "Download current yank to /tmp/tmp-vids/.
Afterwards try to push it to the phone with adb"
  (interactive)
  (unless
      (file-exists-p
       "/tmp/tmp-vids/")
    (make-directory
     "/tmp/tmp-vids/"))
  (let ((default-directory "/tmp/tmp-vids/"))
    (--each
        (directory-files-recursively "/tmp/tmp-vids" ".*")
      (delete-file
       it))
    (team/proc-with-cb
     (start-process
      "yt-dl"
      "*yt-dl*"
      "youtube-dl"
      (evil-get-register ?\"))
     (benj-phone-push
      (directory-files-recursively
       "/tmp/tmp-vids/"
       ".*")))))

(defun benj/play-vlc (&optional file)
  "Play a vid with vlc."
  (interactive)
  (start-process "play-vlc" "*play-vlc*" "vlc"
                 (or file (read-file-name "Play vid: " "~/Videos/" nil nil nil
                                          (lambda (file) (member (file-name-extension file) '("mp4" "mkv" "webm")))))))

(defun benj/play-vlc-any (file)
  "See `benj/play-vlc'"
  (interactive"fPlay vid: ")
  (benj/play-vlc file))

(defun benj/play-vlc-last ()
  "See `benj/play-vlc'"
  (interactive)
  (benj/play-vlc (latest-file "~/Videos/")))
