(defconst benj-trello-leader-keys "ot")

(spacemacs/declare-prefix benj-trello-leader-keys "trello")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-trello-leader-keys (car x)) (cdr x)))
        '(
          ("y" . benj-trello-copy-card-as-yank)
          )))
