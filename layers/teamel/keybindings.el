






(defconst teamel-leader-keys "oi")

(spacemacs/declare-prefix teamel-leader-keys "team-funcs")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat teamel-leader-keys (car x)) (cdr x)))
        '(("l" . teamel-view-log)
          ("b" . teamel-add-debug-button)
          ("B" . teamel-add-debug-button-with-region)
          ("S" . teamel/yank-idlegame-sln))))
