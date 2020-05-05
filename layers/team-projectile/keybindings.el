
(defconst team-projectile-leader-keys "op")

(spacemacs/declare-prefix team-projectile-leader-keys "project")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat team-projectile-leader-keys (car x)) (cdr x)))
        '(("f" . team-helm-projectile-find-many-files)
          ("p" . team-idlegame-find-prefabs))))
