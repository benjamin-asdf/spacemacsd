(defconst backups-leader-keys "of")

(spacemacs/declare-prefix backups-leader-keys "files etc.")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat backups-leader-keys (car x)) (cdr x)))
        '(("b" . benj-backup/make-curr-buff-backup)
          )))
