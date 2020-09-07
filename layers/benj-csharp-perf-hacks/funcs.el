
(defmacro benj/count-step (sym step)
  "Increment SYM value, if STEP amount of times reached, return non nil,
nil otherwise"
  `(= (% (setq ,sym (1+ ,sym)) step) 0))

(defvar benj-chsarp/c-after-change-counter 0
  "Increment everytime `c-after-change' would have been called but was blocked.
Every x time, actually call the func.")

(defun benj-csharp/c-after-change-adv (orig-func &rest args)
  "Advice around `c-after-change'. Throttle, if in big buffer."
  (when (or (< (point-max) 5000)
            (benj/count-step benj-chsarp/c-after-change-counter 30))
    (apply orig-func args)))

(advice-add 'c-after-change :around #'benj-csharp/c-after-change-adv)

(spacemacs/set-leader-keys "ofl" 'font-lock-fontify-buffer)
