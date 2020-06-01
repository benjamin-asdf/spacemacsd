
;; proof of concept this would be a hammer to make big files zappy
;; other one would be `jit-lock-function'
;; maybe I can have a counter and call c after change every 30 inputs
;; if the buffer exceeds a certain size


(defvar benj-chsarp/c-after-change-counter 0
  "Increment everytime `c-after-change' would have been called but was blocked.
Every x time, actually call the func.")

(defun benj-csharp/c-after-change-adv (orig-func &rest args)
  "Advice around `c-after-change'. Throttle, if in big buffer."
  (when (or (< (point-max) 5000)
            (= (% (setq benj-chsarp/c-after-change-counter (1+ benj-chsarp/c-after-change-counter)) 30) 0))
    (apply orig-func args)))

(advice-add 'c-after-change :around #'benj-csharp/c-after-change-adv)

;; (advice-remove 'c-after-change #'benj-csharp/c-after-change-adv)

(spacemacs/set-leader-keys "ofl" 'font-lock-fontify-buffer)
