;; Constants
(defconst minder-private-repo-root "~/.local/minder/"
  "Root of minders private journal repo.")

(defconst minder-meme-journal-dir (concat minder-private-repo-root "memetic-journal/")
  "The directory minder saves memetic journal files.")

(defconst minder-nogo-messages-file (concat minder-private-repo-root "nogo-messages"))


(defconst minder-imaginary-deeds
  '(
    ("Just connected 5% more of population to brain computer interface." "Just Built deep sea outpost."
     "Just harnessed ancient alien library." "Just did a deed." "Just accomplished something." "Just Inclined mind towards winning."
     "Just took a step." "Just won." "Just updated some memetic program." "Just got rid of a few lines of neurol link code." "Just updated gas giant outpost database."))
  "Deeds used for `minder-do-deed' cons cells correspond to the deed level.")

(defconst minder-mined-asteriod-message "Mined an asteriod."
  "The default basic deed.")

(defconst minder-food-request-wait-time 15
  "The wait time in minutes between requesting food and allowing food.")

(defconst minder-think-about-food-duration 10)

;; Variables

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
  (minder--push-message minder-mined-asteriod-message))


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

(defun minder--push-nogo-message ()
  "Random message read from `minder-nogo-messages-file'."
  (minder--push-message (rand-element (benj-read-lines minder-nogo-messages-file))))


;; automatically save it and push it to a repo maybe
