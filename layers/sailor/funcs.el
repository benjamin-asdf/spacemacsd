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

(defun sailor-dump-find-declaration ()
  "Start stupid search for class or enum declarion syntax for thing at point."
  (interactive)
  (sailor-rg-search-in-project
   (format "\\b((class)|(enum))\\s+%s\\b" (thing-at-point 'evil-word))))

(defun sailor-dump-find-comp-declaration ()
  "Start search for component declaration syntax for thing at point."
  (interactive)
  (sailor-rg-search-in-project
   (format "\\bclass\\s+%s\\s+:.*Component" (thing-at-point 'evil-word))))


(defun sailor-get-pet-id-with-name (name)
  (interactive "sPet name: ")
  (with-temp-buffer
    (insert-file-contents "c:/ClashOfStreamers/IdleGame/Assets/#/Sources/Expansion/Shared/PetsMetaData.asset")
    (search-forward-regexp (format "%s.*\n.*id: \\(.*\\)" name) nil t)
    (message (match-string 1))
    (kill-new (match-string 1))))


;; maybe

;; - Search normal compo used
;; - Search where comp used else?

;; (defconst sailor-entitas-syntax
;;   '(('entitas-set . "Set")
;;     ('entitas-add . "Add")
;;     ('entitas-replace . "Replace")
;;     ('entitas-remove . "Remove")))



;; (defconst sailor-component-declarations '("Component" "UniqueFlagComponent" "UniqueComponent" "PrimaryIndexComponent" "IndexComponent"))




;; (defun sailor-component-declerations-canditates ()
;;   "List of all component declarations found with global.
;; Component declartion is a plist containing the the keys
;; `:path' - The absolute path of the declaration location,
;; `:line' - The line number of the location,
;; `:name' - The name of the component."

;;   (let ((global-cmd )) )



;;   )

;; (defun sailor--global-find-refs (name)

;;   ((let default-directory idlegame-project-root)
;;    (shell-command-to-string (format "global --rerence --result=grep %s" (shell-quote-argument name))))

;;   )

;; ;; somehting with
;; ;; global --reference --result=grep "UniqueFlagComponent"


;; (defvar helm-sailor-component-declerations-source
;;   (helm-build-sync-source "Sailor Component Declarations"
;;     :candidates (sailor-component-declerations-canditates)))

;; (helm :sources helm-sailor-component-declerations-source :buffer "*helm-sailor-components*")


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





(defconst sailor--insta-react-template "
      AddInstantReact(Matcher.AllOf<%s>().AddedOrRemoved(), e => {
              Debug.Log($\"set %1$s to {e.%2$s<%1$s>()}\");
      });
")

(defun sailor-copy-with-instant-react-template (name flagcomp)
  "Copy a template for instant react as kill. flagcomp"
  (evil-set-register ?i (format sailor--insta-react-template name (if flagcomp "Is" "Has"))))


(defun sailor-instant-react-template-to-reg-i (&optional flagcomp)
  "Add to register an instant react template. If FLAGCOMP in non nil, use flag template."
  (interactive)
  (sailor-copy-with-instant-react-template (thing-at-point 'evil-word) flagcomp))

(defun sailor-instant-react-to-reg-flagcomp () (interactive) "See `sailor-instant-react-template-to-reg-i'" (sailor-instant-react-template-to-reg-i t))
