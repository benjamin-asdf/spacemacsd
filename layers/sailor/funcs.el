

(defun sailor-find-flag-set-all (&optional word)
  "Maybe find spots where flag is set true or false.
See sailor-find-flag-set"
  (interactive "P")
  (sailor-find-flag-set word t))


(defun sailor-find-flag-set (&optional word all)
  "Maybe find a spots where a flag is set.
If ALL is non nil, also search the flag being set to false.
Use rg to search cs files in project for Entitas syntax."
  (interactive "P")
  (unless word
    (setq word (thing-at-point 'evil-word)))
  (sailor-rg-search-in-project
   (format "\\.Set<%s>\\(%s\\)" word (if all "\.*" "true"))))

(defun sailor-find-comp-set (&optional word)
  "Start a search for entitas set component syntax.
With WORD as component. Defaults to thing at point."
  (interactive "P")
  (unless word
    (setq word (thing-at-point 'evil-word)))
  (sailor-rg-search-in-project
   (format "\\.\\w+<%s>\\(\\w+\\)" word)))


;; maybe

;; - Search normal compo used
;; - Search where comp used else?

;; (defconst sailor-entitas-syntax
;;   '(('entitas-set . "\\.Set")
;;     ('entitas-add . "\\.Add")
;;     ('entitas-replace . "\\.Replace")
;;     ('entitas-remove . "\\.Remove")))
