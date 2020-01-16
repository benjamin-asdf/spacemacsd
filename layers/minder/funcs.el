;; Constants
(defconst minder-private-repo-root "~/.local/minder/"
  "Root of minders private journal repo.")

(defconst minder-meme-journal-dir (concat minder-private-repo-root "memetic-journal/")
  "The directory minder saves memetic journal files.")

(defconst minder-sounds-dir (concat minder-private-repo-root "sounds-lookup/")
  "The directory that minder checks for sound lookup files")

;; TODO I guess I should eval an sexpr there instead of having a file with lines.
(defconst minder-remembered-msgs-file (concat minder-private-repo-root "remember-that")
  "The file minder stores remembered msgs.")

(defconst minder-sounds-types
  '((minder-friendly-sounds . "friendly-sounds")
    (minder-intense-sounds . "intense-sounds")
    (minder-intense-sounds-long . "intense-sounds-long")
    (minder-mining-sounds . "mining-sounds")
    (minder-rock-breaks-sounds . "rock-breaks")
    (minder-abstract-sounds . "abstract-sounds"))
  "Types of minder sounds to play. There must be a files for every type,
in the format described in `minder-play-sound'")

(defconst minder-nogo-messages-file (concat minder-private-repo-root "nogo-messages")
  "The file minder looks for nogo messages")

(defconst minder-imaginary-deeds
  '(("Just did a deed." "Just accomplished something." "Just Inclined mind towards winning."
     "Just took a step." "Just won." "Just updated some memetic program." "Just activated some rocket activation patterns."
     "Just got some dopamine, benj style." "Just took a breath." "Just used the brain." "Like a sip of water."
     "Just took the best thing of the momemt."))
  "Deeds used for `minder-do-deed' cons cells correspond to the deed level.")

(defconst minder-mined-asteriod-message "Mined an asteriod."
  "The default basic deed.")

(defconst minder-something-random-request-wait-time 10
  "The wait time in minutes between requesting something-random and allowing something-random.")

(defconst minder-think-about-something-random-duration 8)

;; Variables

(defvar minder-last-pushed-msg nil)

(defvar minder-something-random-request-allowed-time nil)

;; TODO sore in cache file.
(defvar minder-thought-about-something-random-today nil)

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

(defun minder-push-best-message (&optional msg)
  "Push MSG or one of .config/my-messages to minder file. "
  (interactive)
  (minder-play-sound 'minder-abstract-sounds)
  (minder--push-message (or msg (benj-best-message))))

(defun minder-request-something-random ()
  "Ask for permission to eat"
  (interactive)
  (minder--push-message "Hey minder, am I allowed to eat now?")
  (cond ((null minder-something-random-request-allowed-time)
         (minder--set-something-random-allowed-soon))
        ((not (time-less-p minder-something-random-request-allowed-time (current-time)))
         (message "Not yet, take 3 breaths."))
        ((not (and (yes-or-no-p "Did you take 3 breaths?") (yes-or-no-p "Did you REALY take 3 breaths?")))
         (minder--set-something-random-allowed-soon 0.33))
        (t
         (minder--push-message "You are allowed to think about something random now.")
         (setq minder-something-random-request-allowed-time nil))))

(defun minder-abort-something-random-request ()
  "Abort something-random request, if one is ongoing. Inform user."
  (interactive)
  (if minder-something-random-request-allowed-time
      (progn (minder--push-message
              (format "Aborted something random, would have been allowed at %s"
                      (minder--something-random-allowed-time-formatted)))
             (setq minder-something-random-request-allowed-time nil))
    (message "No something random request for aborting")))

(defun minder--set-something-random-allowed-soon (&optional factor)
    "Set something-random allowed in some time in the future.
Depends on `minder-something-random-request-wait-time'.
If non-nil, modify wait time by FACTOR."
  (setq minder-something-random-request-allowed-time (time-add (current-time) (* minder-something-random-request-wait-time 60 (or factor 1))))
  (minder--push-message (format "Take 3 breaths, and ask me again in %.2f minutes, at %s."
                                (/ (- (float-time minder-something-random-request-allowed-time) (float-time (current-time))) 60)
                                (minder--something-random-allowed-time-formatted))))

(defun minder--something-random-allowed-time-formatted ()
  "Formatted time string for `minder-something-random-request-allowed-time'."
  (format-time-string "%T" minder-something-random-request-allowed-time))

(defun minder-do-deed (&optional level)
  "Push a message full of accomplishment to memetic journal.
See `minder-push-message'."
  (interactive)
  (minder-play-sound 'minder-intense-sounds-long)
  (minder--push-message (rand-element (nth (or level 0) minder-imaginary-deeds))))

(defun minder-mine-asteriod ()
  "Push `minder-mined-asteriod-message' to memetic journal.
See `minder--push-message'"
  (interactive)
  (minder--play-mine-sounds)
  (minder--push-message minder-mined-asteriod-message t))

(defun minder--play-mine-sounds ()
  (minder-play-sound 'minder-mining-sounds)
  (minder-play-sound 'minder-rock-breaks-sounds))


(defun minder-ask-to-think-about-something-random ()
  "Ask minder to think about something-random.
You are allowed to think about something-random once per day. For `minder-think-about-something-random-duration'."
  (interactive)
  (minder--push-message "Hey minder, am I allowed to think about something random?")
  (if minder-thought-about-something-random-today (minder--push-nogo-message)
    (progn (minder--push-message
      (format "Sure, for %s minutes until %s"
              minder-think-about-something-random-duration
              (format-time-string "%T" (time-add (current-time) (* minder-think-about-something-random-duration 60))))))
    (setq minder-thought-about-something-random-today t)))

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

(defun minder-play-sound (kind)
  "Play a random sound of KIND.
KIND must be one of `minder-sounds-types'. The associated sound file must exist.
The sound file must be a file of absolute paths pointing to .wav files, seperated by newline characters."
  (start-process "minder-play-sound" "*minder-play-sound*" "aplay" (benj-rand-line-from-file (minder-sounds-file kind))))

(defun minder-sounds-file (type)
  "Sounds file name.
TYPE must be one of `minder-sounds-types'"
  (concat minder-sounds-dir (cdr (assoc type minder-sounds-types))))

(defun minder-delete-meme ()
  "Give some imaginary feedback about deleting a meme."
  (interactive)
  (let ((id (random 1000)))
    (if (yes-or-no-p (format "Do you really want to current meme %s?" id))
        (message (format "Deleted meme, was %s" id)))))


;; WIP

(define-derived-mode minder-mode fundamental-mode "Minder"
  "Minder major mode for memetic journal files."
  :syntax-table nil
  :abbrev-table nil
  (evil-define-key 'motion minder-mode-map
    (kbd "J") 'minder-mine-asteriod
    (kbd "L") 'minder-mine-big-asteriod
    (kbd "D") 'minder-do-deed
    (kbd "M") 'minder-push-message
    (kbd "R") 'minder-push-remembered-msgs
    (kbd "A") 'minder-push-best-message)
  ;; maybe I figure out how to reuse the map

  (evil-set-initial-state 'minder-mode 'motion))
;; fix motion state
;; or fix the thing for normal mode

(add-to-list 'auto-mode-alist '("/memetic-journal/" . minder-mode))


;; TODO
(defvar minder-asteroid-taps-made 0)
;; todo random between 3 and 6 or something
(defconst minder-big-asteroid-taps-required 4)
(defun minder-mine-big-asteriod ()
  (interactive)
  (setq minder-asteroid-taps-made (1+ minder-asteroid-taps-made))
  (minder--play-mine-sounds)
  (minder-big-asteriod-progress-message (/ minder-asteroid-taps-made (* minder-big-asteroid-taps-required 1.0)))
  (when (>= minder-asteroid-taps-made minder-big-asteroid-taps-required)
    (progn (minder--push-message "Asteriod mined.")
           (setq minder-asteroid-taps-made 0))))

(defun minder-big-asteriod-progress-message (progress)
  "Push message about asteriod mining progress.
PROGRESS should be a float between 0 and 1."
  (minder-push-message  "Mining asteriod... ")
  (minder-push-message  (minder-progress-bar progress)))

;; TODO make the bar look more different when asteriod minded
(defconst minder-progress-bar-length 40)
(defun minder-progress-bar (progress)
  (let ((progress-part-size (floor (* progress minder-progress-bar-length))))
    (concat (make-string progress-part-size ?#) ">"
            (minder--progress-bar-thin-line (- minder-progress-bar-length progress-part-size)))))

(defun minder--progress-bar-thin-line (lenght)
  "Evaluates to a string like '_ _ _' with LENGTH"
  (let ((s ""))
    (dotimes (i (/ lenght 2) s)
      (setq s (concat s "_ " )))
    s))



;; TODO where to put this or rename this?
;; TODO debug
;; (defvar helm-benj-best-message-source
;;  (helm-build-sync-source "Best Message"
;;    :candidates (benj-read-lines best-messages-file)))

(defun minder-select-a-message ()
 "Use helm to select one of best messages and push it."
 (interactive)
 (minder-push-best-message (format "#!@ - %s" (helm :sources helm-benj-best-message-source :buffer "*become-aware*"))))

;; TODO
;; automatically save it and push it to a repo maybe

(defun minder-good-morning ()
  "Ask the user a series of questions and push into journal file.
Meant to be run at the start of the day."

  )

(defun minder-good-night ()
  "Push some stats about the day into the journal file."
  ;; how many asteriods
  ;; (or how many point the user gathered on that day I guess)
  ;; man I want something that says 3 rockets launched etc lul
  )


;; I want to have one cache file for every month
;; there I have one big (setq) ?

;; TODO start rocked by typing some correct numbers



(defun minder-start-rocket ()
  (interactive)
  (let ((string (number-to-string (random (expt 10 6)))))
   (if (string-equal string (read-string (format "Type %s" string)))
       (message "rocked started.")
     (message "Try again."))))
