;; Constants
(defconst minder-private-repo-root "~/.local/minder/"
  "Root of minders private journal repo.")

(defconst minder-meme-journal-dir (concat minder-private-repo-root "memetic-journal/")
  "The directory minder saves memetic journal files.")

(defconst minder-sounds-dir (concat minder-private-repo-root "sounds-lookup/")
  "The directory that minder checks for sound lookup files")

(defconst minder-cache-dir (concat minder-private-repo-root ".cache/"))

(defconst minder-file-ext ".mind"
  "The extension that should conventionally used for mind files")

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


;; TODO
(defconst minder-sounds-search-terms
  '((minder-friendly-sounds . "crack")
    (minder-intense-sounds . "crack")
    (minder-intense-sounds-long . "hero")
    (minder-mining-sounds . "crack")
    (minder-rock-breaks-sounds . "rockbreaks")
    (minder-abstract-sounds . "poop"))
  "Search-Terms of minder sounds")

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

(defconst minder-food-request-wait-time 10
  "The wait time in minutes between requesting food and allowing food.")

(defconst minder-think-about-food-duration 8)

;; Variables

(defvar minder-last-pushed-msg nil)

(defvar minder-food-request-allowed-time nil)

;; TODO sore in cache file.
(defvar minder-thought-about-food-today nil)


(defvar minder-play-sound-command
  (if (eq system-type 'windows-nt) "benaplay" "aplay"))

(defconst minder-good-morning-template
  "* %s
** Wake up Info
** WorkspotInfo")


(defvar minder-day-streak nil
  "Streak of something that was accomplished for a span of days. This should be an alist with")

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
;; TODO  toggle as abbrev

(defun minder-push-best-message (&optional msg)
  "Push MSG or one of .config/my-messages to minder file. "
  (interactive)
  (minder-play-sound 'minder-abstract-sounds)
  (minder--push-message (or msg (benj-best-message))))

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
         (minder--push-message "You are allowed to think about something random now.")
         (setq minder-food-request-allowed-time nil))))

(defun minder-abort-food-request ()
  "Abort food request, if one is ongoing. Inform user."
  (interactive)
  (if minder-food-request-allowed-time
      (progn (minder--push-message
              (format "Aborted something random, would have been allowed at %s"
                      (minder--food-allowed-time-formatted)))
             (setq minder-food-request-allowed-time nil))
    (message "No something random request for aborting")))

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


(defun minder-ask-to-think-about-food ()
  "Ask minder to think about food.
You are allowed to think about food once per day. For `minder-think-about-food-duration'."
  (interactive)
  (minder--push-message "Hey minder, am I allowed to think about something random?")
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

(defun minder-check-if-sunday ()
  "Checks if it is sunday today and pushes a message"
  (when (= (decoded-time-weekday (decode-time (current-time))) 0)
    (minder--push-message "It is sunday today.")))

(defun minder-play-sound (kind)
  "Play a random sound of KIND.
KIND must be one of `minder-sounds-types'. The associated sound file must exist.
The sound file must be a file of absolute paths pointing to .wav files, seperated by newline characters."
  (minder--ensure-sound-file kind)
  (start-process "minder-play-sound" "*minder-play-sound*" minder-play-sound-command (or (benj-rand-line-from-file (minder-sounds-file kind)) "")))

(defun minder-invalidate-sounds-lookup ()
  "Clear minder sounds lookups."
  (interactive)
  (benj-clear-directory-contents minder-sounds-dir)
  (message (format "Cleared contents of %s" minder-sounds-dir)))

(defun minder--ensure-sound-file (kind)
  "Initialize a sounds file for KIND. KIND must be one of `minder-sounds-types'."
  (let ((file (minder-sounds-file kind)))
    (unless (file-exists-p file)
      (write-region
       (shell-command-to-string
        (format "fd -I -e wav %s %s"
                (cdr (assoc kind minder-sounds-search-terms))
                (concat (file-name-as-directory idlegame-project-root) "Assets/Audio/_AudioToObject/")))
       nil file))))


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
(defconst minder-mode-font-keywords '(("^\\[.*\\] " . font-lock-variable-name-face)
                                      ("#+?>" . font-lock-type-face)))

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

  (font-lock-add-keywords nil minder-mode-font-keywords)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer))))


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
  (interactive)
  (insert (format minder-good-morning-template (current-time-string)))
  (evil-insert-newline-above)
  (evil-insert-state)
  (indent-according-to-mode))

(defun minder-good-night ()
  "Push some stats about the day into the journal file."
  ;; how many asteriods
  ;; (or how many point the user gathered on that day I guess)
  ;; man I want something that says 3 rockets launched etc lul
  (interactive)
  (minder-push-message (format "Good night. Don't forget the blanket, earplugs etc. Pomodoros today: %d" org-pomodoro-count))
  (mapcar 'minder-push-message minder-good-night-messages))


;; I want to have one cache file for every month
;; there I have one big (setq) ?

(defvar minder-good-night-messages '()
  "Some messages that will send on good night.
See `minder-good-night'
;; TODO reset on morning
")

(defun minder-add-good-night-message (arg)
  "Add something to `minder-good-night-messages'"
  (interactive"sEnter good night message: ")
  (push arg minder-good-night-messages))

(defvar minder-last-rocked-string nil)

(defun minder-start-rocket ()
  "Ask the user to type in a string of numbers. And put success or failure messages as minder messages."
  (interactive)
  (setq minder-last-rocked-string (or minder-last-rocked-string (number-to-string (random (expt 10 6)))))
  (if (string-equal minder-last-rocked-string (read-string (format "Type %s" minder-last-rocked-string)))
      (progn (message "rocked started.")
             (minder-play-sound 'minder-intense-sounds)
             (setq minder-last-rocked-string nil))
    (message "Try again.")))


(defun minder-take-a-breath ()
  "Do a minder deed after some time.
Use this to be able to close the eyes for 20 seconds and take a deep breath."
  (interactive)
  (minder-push-message "Take a breath.")
  (run-at-time "20" nil (lambda () (progn (minder-do-deed) (minder-push-message "Success.")))))


(defun minder-persist-data (symbol value day)
  "Save SYMBOLs value VALUE into the current cache file."
  )



(defvar minder-take-bite-messages
  '("Small spoons" "32 chews at least" "Slow and deliberate" "Carefully")
  "Messages for `minder-take-bite'")


(defun minder-take-bite ()
  "Show a message defined in `minder-take-bite-messages'. Meant to make the user eat efficiently."
  (interactive)
  (minder-push-message (rand-element minder-take-bite-messages)))
