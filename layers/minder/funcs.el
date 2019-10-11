;; Constants
(defconst minder-private-repo-root "~/.local/minder/"
  "Root of minders private journal repo.")

(defconst minder-meme-journal-dir (concat minder-private-repo-root "memetic-journal/")
  "The directory minder saves memetic journal files.")

(defconst minder-remembered-msgs-file (concat minder-private-repo-root "remember-that"))

(defconst minder-nogo-messages-file (concat minder-private-repo-root "nogo-messages"))


(defconst minder-imaginary-deeds
  '(
    ("Just connected 5% more of population to brain computer interface." "Just Built deep sea outpost."
     "Just harnessed ancient alien library." "Just did a deed." "Just accomplished something." "Just Inclined mind towards winning."
     "Just took a step." "Just won." "Just updated some memetic program." "Just got rid of a few lines of neural link code." "Just updated gas giant outpost database." "Just activated some rocket activation patterns."))
  "Deeds used for `minder-do-deed' cons cells correspond to the deed level.")

(defconst minder-mined-asteriod-message "Mined an asteriod."
  "The default basic deed.")

(defconst minder-food-request-wait-time 10
  "The wait time in minutes between requesting food and allowing food.")

(defconst minder-think-about-food-duration 8)

;; Variables

(defvar minder-last-pushed-msg nil)

(defvar minder-food-request-allowed-time nil)

;; TODO sore in cache file.
(defvar minder-thought-about-food-today nil)

;; Functions

(defun minder--todays-journal-file ()
  "Get the push message file for the current day."
  (concat (file-name-as-directory minder-meme-journal-dir) (format-time-string "%F")))

(defun minder-push-message (msg)
  "Open the push messages file and push MSG."
  (interactive "sWhat is the current meme?")
  (minder--push-message msg))

(defun minder--push-message (msg &optional non-intrusive)
  "Open todays journal file and push MSG.
If NON-INTRUSIVE is non nil, supress opening a journal file window."
  (setq minder-last-pushed-msg msg)
  (let ((curr-buffer (buffer-name)))
    (cond ((string-equal (concat (file-name-as-directory minder-meme-journal-dir) curr-buffer)
                         (minder--todays-journal-file))
           (minder--insert-message msg)
           (save-buffer))
          (non-intrusive
           (progn (benj-append-to-file
                   (minder--todays-journal-file) (minder--formatted-msg msg))
                  (message msg)))
          (t (progn (find-file-other-window (minder--todays-journal-file))
                  (minder--insert-message msg)
                  (save-buffer)
                  (switch-to-buffer-other-window curr-buffer))))))

(defun minder--insert-message (msg)
  "Insert new line containing a timestamp and MSG"
  (goto-char (point-max))
  (insert (minder--formatted-msg msg)))

(defun minder--formatted-msg (msg)
  "Format MSG for minder memetic journal."
  (format "[%s] %s\n" (format-time-string "%T") msg))

(defun minder-push-best-message ()
  "Push one of messages defined in .config/my-messages to minder file."
  (interactive)
  (minder--push-message (benj-best-message)))

(defun minder-request-food ()
  "Ask for permission to eat"
  (interactive)
  (minder--push-message "Hey minder, am I allowed to eat now?")
  (cond ((null minder-food-request-allowed-time)
         (minder--set-food-allowed-soon))
        ((not (time-less-p minder-food-request-allowed-time (current-time)))
         (message "Not yet, take 3 breaths."))
        ((not (and (yes-or-no-p "Did you take 3 breaths?") (yes-or-no-p "Did you REALY take 3 breaths?")))
         (minder--set-food-allowed-soon 0.33))
        (t
         (minder--push-message "You are allowed to eat now.\n Do not forget to do some pushups!")
         (setq minder-food-request-allowed-time nil))))

(defun minder-abort-food-request ()
  "Abort food request, if one is ongoing. Inform user."
  (interactive)
  (if minder-food-request-allowed-time
      (progn (minder--push-message
              (format "Aborted food, would have been allowed at %s"
                      (minder--food-allowed-time-formatted)))
             (setq minder-food-request-allowed-time nil))
    (message "No food request for aborting")))

(defun minder--set-food-allowed-soon (&optional factor)
    "Set food allowed in some time in the future.
Depends on `minder-food-request-wait-time'.
If non-nil, modify wait time by FACTOR."
  (setq minder-food-request-allowed-time (time-add (current-time) (* minder-food-request-wait-time 60 (or factor 1))))
  (minder--push-message (format "Take 3 breaths, and ask me again in %.2f minutes, at %s."
                                (/ (- (float-time minder-food-request-allowed-time) (float-time (current-time))) 60)
                                (minder--food-allowed-time-formatted))))

(defun minder--food-allowed-time-formatted ()
  "Formatted time string for `minder-food-request-allowed-time'."
  (format-time-string "%T" minder-food-request-allowed-time))

(defun minder-do-deed (&optional level)
  "Push a message full of accomplishment to memetic journal.
See `minder-push-message'."
  (interactive)
  (minder--push-message (rand-element (nth (or level 0) minder-imaginary-deeds))))

(defun minder-mine-asteriod ()
  "Push `minder-mined-asteriod-message' to memetic journal.
See `minder--push-message'"
  (interactive)
  (minder--push-message minder-mined-asteriod-message t))


(defun minder-ask-to-think-about-food ()
  "Ask minder to think about food.
You are allowed to think about food once per day. For `minder-think-about-food-duration'."
  (interactive)
  (minder--push-message "Hey minder, am I allowed to think about food?")
  (if minder-thought-about-food-today (minder--push-nogo-message)
    (progn (minder--push-message
      (format "Sure, for %s minutes until %s"
              minder-think-about-food-duration
              (format-time-string "%T" (time-add (current-time) (* minder-think-about-food-duration 60))))))
    (setq minder-thought-about-food-today t)))

(defun minder-friendly-nogo ()
  "Push a small conversion with minder. Get told a nogo message."
  (interactive)
  (minder--push-message "Hey minder, ....?")
  (minder--push-nogo-message))

(defun minder--push-nogo-message ()
  "Push a random message read from `minder-nogo-messages-file'."
  (minder--push-message (rand-element (benj-read-lines minder-nogo-messages-file))))


(defun minder-remember-last-msg ()
  "Store `minder-last-pushed-msg' in a special notes file."
  (interactive)
  (if minder-last-pushed-msg
      (progn (benj-append-to-file minder-remembered-msgs-file minder-last-pushed-msg t)
             (minder--push-message "- Inclining the mind to remember that!"))
    (message "There is no last message to remember.")))

(defun minder-push-remembered-msgs ()
  "Push 3 random remembered msgs into current journal file."
  (interactive)
  (when (file-exists-p minder-remembered-msgs-file)
      (dotimes (i 3)
        (minder--push-message (benj-rand-line-from-file minder-remembered-msgs-file)))))

;; automatically save it and push it to a repo maybe
