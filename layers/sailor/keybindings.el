







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
          ("c" . sailor-find-comp-set)
          ("v" . sailor-find-comp-value-access)
          ("i" . sailor-dump-find-cs-implementations)
          ("m" . sailor-find-comp-matched)
          ("d" . sailor-dump-find-comp-declaration)
          ("D" . sailor-dump-find-declaration)
          ("r" . sailor-instant-react-template-to-reg-i)
          ("R" . sailor-instant-react-to-reg-flagcomp))))
