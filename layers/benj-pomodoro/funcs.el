(with-eval-after-load 'org-pomodoro
  (setq-default org-pomodoro-manual-break t)
  (setq-default org-pomodoro-play-sounds nil)
  (setq-default org-pomodoro-short-break-length 3)
  (setq-default org-pomodoro-audio-player (or org-pomodoro-audio-player (executable-find "benaplay"))))




(defadvice org-pomodoro-update-mode-line (around my/org-pom-modeline-adv activate)
  "See `org-pomodoro-update-mode-line'
Default update mode line but don't force modeline update"
  (let ((s (cl-case org-pomodoro-state
             (:pomodoro
              (propertize org-pomodoro-format 'face 'org-pomodoro-mode-line))
             (:overtime
              (propertize org-pomodoro-overtime-format
                          'face 'org-pomodoro-mode-line-overtime))
             (:short-break
              (propertize org-pomodoro-short-break-format
                          'face 'org-pomodoro-mode-line-break))
             (:long-break
              (propertize org-pomodoro-long-break-format
                          'face 'org-pomodoro-mode-line-break)))))
    (setq org-pomodoro-mode-line
          (when (and (org-pomodoro-active-p) (> (length s) 0))
            (list "[" (format s (org-pomodoro-format-seconds)) "] "))))
  ;; (force-mode-line-update t)
  )
