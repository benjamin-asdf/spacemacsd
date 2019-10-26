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

(defun sailor-find-comp-value-access ()
  "Start search for value access syntax with thing at point."
  (interactive)
  (sailor-rg-search-in-project
   (format "\\.((Get)|(Is))<%s>\\(\\)\(\\.value\)?" (thing-at-point 'evil-word))))

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

(defun sailor-dump-find-class-declaration ()
  "Start stupid search for class decleration syntax for thing at point."
  (interactive)
  (sailor-rg-search-in-project
   (format "\\bclass\\s+%s" (thing-at-point 'evil-word))))

(defun sailor-dump-find-comp-declaration ()
  "Start search for component declaration syntax for thing at point."
  (interactive)
  (sailor-rg-search-in-project
   (format "\\bclass\\s+%s\\s+:.*Component" (thing-at-point 'evil-word))))


;; maybe

;; - Search normal compo used
;; - Search where comp used else?

;; (defconst sailor-entitas-syntax
;;   '(('entitas-set . "Set")
;;     ('entitas-add . "Add")
;;     ('entitas-replace . "Replace")
;;     ('entitas-remove . "Remove")))



;; (defconst sailor-component-declarations '("Component" "UniqueFlagComponent" "UniqueComponent" "PrimaryIndexComponent" "IndexComponent"))



;; (defun sailor-global-get-references ()

;;   )

;; somehting with
;; global --reference --result=grep "UniqueFlagComponent"





(defconst sailor-fd-find-alot "fd -H -E=.git -I -tf -0 .")

;; TODO helm search behavoiur wrong
(with-eval-after-load 'projectile
  (defun sailor-projectile-find-file-all ()
    "Simple implementation of projectile find file without any file filters"
    (interactive)

    (let* ((project-root (projectile-ensure-project (projectile-project-root)))
           (files (projectile-files-via-ext-command project-root sailor-fd-find-alot))
           (file (projectile-completing-read "Find file: "
                                             files))
           (ff (or ff-variant #'find-file)))
      (when file
        (funcall ff (expand-file-name file project-root))
        (run-hooks 'projectile-find-file-hook)))))
