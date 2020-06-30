(defconst surfel/leader-keys "ow")

(spacemacs/declare-prefix surfel/leader-keys "web")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat surfel/leader-keys (car x)) (cdr x)))
        '(("t" . surfel/open-best-trello-board)
          ("g" . surfel/search)
          ("r" . surfel/search-for-region))))
