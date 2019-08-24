


(defconst bunel-leader-keys "ou")

(spacemacs/declare-prefix bunel-leader-keys "unity")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat bunel-leader-keys (car x)) (cdr x)))
        '(("s" . bunel-save-and-refresh)
          ("c" . bunel-refresh-client)
          ("a" . bunel-refresh-all))))
