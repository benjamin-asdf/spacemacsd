


(defconst bunel-leader-keys "ou")

(spacemacs/declare-prefix bunel-leader-keys "unity")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat bunel-leader-keys (car x)) (cdr x)))
        '(("s" . bunel-save-and-refresh)
          ("S" . (lambda () (interactive) (bunel-save-and-refresh "with-playmode")))
          ("c" . bunel-refresh-client)
          ("C" . (lambda () (interactive) (bunel-refresh-client "with-playmode")))
          ("a" . bunel-refresh-all)
          ("A" . (lambda () (interactive) (bunel-refresh-all "with-playmode"))))))