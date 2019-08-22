







(defconst sailor-leader-keys "os")

(spacemacs/declare-prefix sailor-leader-keys "search")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat sailor-leader-keys (car x)) (cdr x)))
        '(("p" . rg-dwim-project-dir)
          ("d" . rg-dwim-current-dir)
          ("s" . sailor-rg-search-in-project)
          ("f" . sailor-find-flag-set)
          ("F" . sailor-find-flag-set-all)
          ("C" . sailor-find-comp-set))))
