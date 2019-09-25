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
   (format "\\.((Add)|(Set)|(Replace))<%s>\\(" word)))

(defun sailor-dump-find-cs-implementations ()
  "Start search with cs implementation syntax for thing at point."
  (interactive)
  (sailor-rg-search-in-project
   (format "\\w+\\s+%s\\(.*\\{" (thing-at-point 'evil-word))))


(defun sailor-find-comp-matched ()
  "Search for matcher syntax with things at point."
  (interactive)
  (sailor-rg-project-multiline (sailor--matcher-syntax (thing-at-point 'evil-word))))

;; TODO
;; (defun sailor-find-react-to-comp ()
;;   "Search for reactive system syntax with thing at point."
;;   (interactive)
;;   )

(defun sailor--matcher-syntax (comp)
  "Get matcher syntax for COMP."
  (format "Matcher(\n)?(\n\r)?\.(\n)?(\n\r)?.*%s\\b" comp))


;; maybe

;; - Search normal compo used
;; - Search where comp used else?

;; (defconst sailor-entitas-syntax
;;   '(('entitas-set . "Set")
;;     ('entitas-add . "Add")
;;     ('entitas-replace . "Replace")
;;     ('entitas-remove . "Remove")))