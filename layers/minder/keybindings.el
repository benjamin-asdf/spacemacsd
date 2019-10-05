(defconst minder-leader-keys "oa")

(spacemacs/declare-prefix minder-leader-keys "mind")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat minder-leader-keys (car x)) (cdr x)))
        '(("a" . minder-push-best-message)
          ("m" . minder-push-message)
          ("e" . minder-abort-food-request)
          ("d" . minder-do-deed)
          ("j" . minder-mine-asteriod)
          ("q" . minder-friendly-nogo)
          ("&" . minder-ask-to-think-about-food)
          ("r" . minder-remember-last-msg)
          ("R" . minder-push-remembered-msgs))))
