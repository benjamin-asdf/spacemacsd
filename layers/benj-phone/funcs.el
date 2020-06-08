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

(defun benj-phone-push-to-misc-dir (file)
  "Push FILE to `benj-phone-misc-dir'"
  (interactive "fFile to push: ")
  (benj-phone-push file benj-phone-misc-dir))


(defun benj/youtube-dl-download-yank ()
  "Download current yank into ~/Vidoes."
  (interactive)
  (let ((default-directory "~/Videos"))
    (async-shell-command (format "youtube-dl \"%s\"" (evil-get-register ?\")))))


(defun benj/play-vlc ()
  "Play a vid with vlc."
  (interactive)
  (start-process "play-vlc" "*play-vlc*" "vlc"
                 (read-file-name "Play vid: " "~/Videos/" nil nil nil
                                 (lambda (file) (member (file-name-extension file) '("mp4" "mkv" "webm"))))))
