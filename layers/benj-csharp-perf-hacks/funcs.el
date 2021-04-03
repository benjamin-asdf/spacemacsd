





(defmacro incfmod (sym step)
  "Increment SYM value, if STEP amount of times reached, return non nil,
nil otherwise"
  `(= (% (setf ,sym (1+ ,sym)) ,step) 0))

(defvar benj-chsarp/c-after-change-counter 0
  "Increment everytime `c-after-change' would have been called but was blocked.
Every x time, actually call the func.")

(defun benj-csharp/c-after-change-adv (orig-func &rest args)
  "Advice around `c-after-change'. Throttle, if in big buffer."
  (when (or (< (point-max) 5000)
            (incfmod benj-chsarp/c-after-change-counter 30))
    (apply orig-func args)))

(advice-add 'c-after-change :around #'benj-csharp/c-after-change-adv)

(spacemacs/set-leader-keys "ofl" 'font-lock-fontify-buffer)

(defadvice imenu-list-update (around my-imenu-update-adv activate)
  (unless
      (or
       (eq major-mode 'magit-process-mode)
       (and (eq major-mode 'chsarp-mode)
            ;; (not benj-imenu-manual)
            ))
    ad-do-it))



(defadvice omnisharp-eldoc-function (around my-omnisharp-eldoc-adv activa)
  (unless
      (>
       (line-number-at-pos
        (point-max))
       1000)
    ad-do-it))

(defun omnisharp--imenu-make-marker (element))
