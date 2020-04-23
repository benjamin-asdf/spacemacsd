(with-eval-after-load 'org-pomodoro
  (setq-default org-pomodoro-manual-break t)
  (setq-default org-pomodoro-short-break-length 3)
  (setq-default org-pomodoro-audio-player (or org-pomodoro-audio-player (executable-find "benaplay"))))
