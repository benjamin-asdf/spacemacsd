;; -*- lexical-binding: t; -*-

(require 'enums)
(require 'unity-labels)
(require 'unity-asset-usages "/home/benj/.spacemacs.d/lisp/unity-asset-usages.el")
(require 'idlegame-definitions)
(require 'csharp-parsing "/home/benj/.spacemacs.d/layers/team-csharp/chsarp-parsing.el")
(require 'team-utils)


(defconst
  spriteloaders-skip-files
  '("#/Sources/Loading/LoadingExt.cs"
    "#/Sources/Loading/LoadingExt.cs"
    "#/Sources/Expansion/Shared/PetLoading/PetLoadingExtensions.cs"
    "#/Sources/ResourceManagement/Monobehaviours/ResolveSprites.cs"
    "#/Sources/ResourceManagement/ResourceManagementExtentions.cs"
    "#/Design/RewardItemAnimationsNew/RewardAnimationSetter.cs"
    "#/Sources/HotStories/Forum/MonoBehaviours/Badges/BadgeGroup.cs"
    "#/Sources/HotStories/Forum/MonoBehaviours/PolymorphicTapToCycle.cs"
    "#/Sources/WebVideos/IPopOutButton.cs"))
(setq
  spriteloaders-skip-files
  '("#/Sources/Loading/LoadingExt.cs"
    "#/Sources/Expansion/Shared/PetLoading/PetLoadingExtensions.cs"
    "#/Sources/ResourceManagement/Monobehaviours/ResolveSprites.cs"
    "#/Sources/ResourceManagement/ResourceManagementExtentions.cs"
    "#/Design/RewardItemAnimationsNew/RewardAnimationSetter.cs"
    "#/Sources/HotStories/Forum/MonoBehaviours/Badges/BadgeGroup.cs"
    "#/Sources/HotStories/Forum/MonoBehaviours/PolymorphicTapToCycle.cs"
    "#/Sources/HotStories/Forum/MonoBehaviours/Posts/Twitter/TweetTopInfo.cs"
    "#/Sources/WebVideos/IPopOutButton.cs"))

(defconst online-loader-names (concat cos-dir "/LoadingIdlegame/Packages/LoadingIdlegame/Runtime/online-loader-names"))
(defconst offline-loader-names (concat cos-dir "/LoadingIdlegame/Packages/LoadingIdlegame/Runtime/offline-loader-names"))




(cl-defun mk-hash-from-list (list &rest args)
  (let ((res
         (eval `(make-hash-table ,@(or args nil)))))
    (dolist (elm list)
      (puthash elm t res))
    res))

(defvar-local cos-investigated-file nil)

(defmacro cos-investigate-file (file &rest body)
  `(team/check-file
    ,file
    (setq cos-investigated-file ,file)
    ,@body))

(defmacro cos-investigate-files (files &rest body)
  `(--each
       ,files
     (cos-investigate-file
      it
      ,@body)))

(defun chsarp-put-statement-one-line ()
  (interactive)
  (join-line
   nil
   (point)
   (save-excursion
     (skip-chars-forward "^;")
     (point-marker))))



(defconst loader-name-file (concat cos-dir "/LoadingIdlegame/Packages/LoadingIdlegame/Runtime/LoaderName.cs"))

(defvar lookup-loaders '())
(defun loader-name-type-lookup (s)
  "If S is a loader defined in `loader-name-file', return one of online or offline, nil otherwise."
  (unless
      (bound-and-true-p lookup-loaders)
    (setq lookup-loaders
          (team/with-file
           loader-name-file
           (let
               ((offline-lut
                 (progn
                   (re-search-forward "LoaderName")
                   (mk-hash-from-list
                    (benj/simple-csharp-enum-values) :test '#'equal)))
                (online-lut
                 (progn
                   (re-search-forward "OnlineLoaderName")
                   (mk-hash-from-list
                    (benj/simple-csharp-enum-values) :test '#'equal))))
             (lambda (that)
               (cond
                ((gethash that online-lut) 'online)
                ((gethash that offline-lut) 'offline)))))))
  (funcall lookup-loaders s))


(defun loader-name--online-loader-p (loader)
  (eq (loader-name-type-lookup loader) 'online))



(defconst
  sprite-container-names-enum-file
  (concat
   idlegame-assets-dir
   "#/Scripts/SpriteContainer.cs"))

(defun sprite-container-names ()
  (team/with-file
   sprite-container-names-enum-file
   (team/collect--reg
    "public const string \\(\\w+\\)" 1)))



(defconst sprite-container-dir (concat idlegame-assets-dir "#/Scripts/View/ScriptableObjects/"))

(defun container-online--p (container)
  (--every?
   (eq 'online
       (unity-asset-usage-asset-type it))
   (-flatten
    (team-unity/detailed-asset-usage
     (or
      (sprite-container-asset-file container)
      (error "%s does not have an asset file" container))))))

(defun sprite-container-asset-file (name)
  (if
      (and (file-exists-p name)
           (file-in-directory-p name sprite-container-dir))
      name
    (id-when
     (concat sprite-container-dir name ".asset.meta")
     #'file-exists-p)))


(defun resolve-sprite-loader-name-with-container-name (container)
  (setq container
        (s-chop-suffixes '(".meta" ".asset") container))
  (-first
   #'loader-name-type-lookup
   (or
    (and
     (s-suffix-p
      "Loader"
      container)
     (list container))
    (--mapcat
     (list (concat it "Loader"))
     (list
      (team/re-replace-in-string
       container
       "Sprites"
       "sSprites"
       t)
      container)))))



(defun resolve-sprite-loader-field (field-name &optional file-name)
  "If FILE-NAME or buffer file is a script that has some SpriteContainer field called FIELD-NAME.
Search the first prefab with the scritp for a sprite container ref,
return the sprite container name."
  (if (bound-and-true-p
       cos-override-resolve-sprite-loader-field-return-value)
      cos-override-resolve-sprite-loader-field-return-value
   (-some-->
       (setq
        file-name
        (or
         file-name
         cos-investigated-file
         buffer-file-name))
     (and
      (file-in-directory-p
       it
       idlegame-assets-dir)
      it)
     (save-excursion
       (->gg)
       (re-search-forward
        (format
         "\\(\\(public\\)\\|\\(\\[SerializeField\\]\\)\\)[[:blank:]]+SpritesContainer[[:blank:]]+?%s[[:blank:]]*;"
         field-name)
        nil t))
     (team-unity/field-ref-search
      field-name
      file-name)
     (s-chop-suffix ".asset.meta" it))))


(defun resolve-sprite-loader-name (container)
  "Try to resolve a sprite loader name from CONTAINER.
Attempt multiple strategies."
  (-some-->
      (or
       (resolve-sprite-loader-name-with-container-name
        (or
         (and
          (string-match "SpriteContainer\\.\\(\\w+\\)" container)
          (match-string 1 container))
         ;; if somebody named some variable exactly like
         ;; the name of a loader, the chance is high this is the correct loader
         (let ((res))
           (team/check-file
            loader-name-file
            (when (re-search-forward
                   container nil t)
              (team/re-this-line
               "\\(\\w+\\),")
              (setq res (match-string 1)))
            nil)
           res)
         (and
          cos-investigated-file
          (resolve-sprite-loader-field
           container))
         container))
       ;; bail out and mark as manual fix
       "TODO")
    (concat
     (if (loader-name--online-loader-p it)
         "OnlineLoaderName."
       "LoaderName.")
     it)))



(defconst online-loaders-dir "Assets/LoadGroups/AssetLoadersOnline/")
(defconst offline-loaders-dir "Assets/LoadGroups/AssetLoaders/")

(defun sprite-container-assets ()
  "Return sprite container asset files inside `sprite-container-dir'."
  (directory-files
   sprite-container-dir
   t
   ".asset$"))

(defun sprite-container-used? (container-or-file)
  "Return non nil if there is either a loader name in the loader enum
for CONTAINER-OF-FILE, or if there are any o"
  (or
   (resolve-sprite-loader-name-with-container-name
    (file-name-base container-or-file))
   (team-unity/any-asset-ref? container-or-file)))


(defun sprite-container-loader-exists? (container)
  "Check if there is an associated loader for CONTAINER."
  (team/with-default-dir
   idlegame-project-root
   (-some-->
       (resolve-sprite-loader-name-with-container-name
        (file-name-base container))
     (concat it ".asset")
     (let ((name it))
       (--any?
        (file-exists-p
         (concat it name))
        (list online-loaders-dir
              offline-loaders-dir))))))


(defun sprite-loaders-refresh-names ()
  "Find SpriteContainers and add new loader names.
Check sprite container assets in the usual directory.
Only add sprite containers if they are actually used."
  (-map
   #'delete-file
   (list online-loader-names offline-loader-names))
  (--map
   (unless
       (resolve-sprite-loader-name-with-container-name it)
     (team/append-new-line
      (if (container-online--p
           it)
          online-loader-names
        offline-loader-names)
      (file-name-base it)))
   (append
    (sprite-container-names)
    (-filter
     #'sprite-container-used?
     (sprite-container-assets)))))


(defun add-loader-names-from-files ()
  "Check `offline-loader-names' and `online-loader-names'.
Put enum syntax into `loader-name-file'.
As side effect reset `lookup-loaders' so it has the updated data next time it is called."
  (cl-flet ((add-loader (type name)
                        (setq name (concat name "Loader"))
                        (unless
                            (save-excursion
                              (re-search-forward name nil t))
                          (->gg)
                          (re-search-forward type)
                          (team-prepend-at-curly
                           (concat name ",") 4)
                          (re-search-forward (format "this %s" type))
                          (team/prepend-at-re
                           "case"
                           (format
                            "case %1$s.%2$s: return nameof(%1$s.%2$s);" type name)
                           12))))
    (team/with-file
     loader-name-file
     (let ((online-name "OnlineLoaderName")
           (offline-name "LoaderName"))
       (--map
        (add-loader online-name it)
        (team/file-lines
         online-loader-names))
       (--map
        (add-loader offline-name it)
        (team/file-lines
         offline-loader-names)))))
  (setq lookup-loaders nil))



(defun cos/fix-loader-names-perf-critical ()
  "Add offline or online loader names for all sprite containers add `loader-name-file'."
  (sprite-loders-refresh-names)
  (add-loader-names-from-files))



(defun replace-sprite-loader-syntax ()
  (let ((dirty))
    (while
        (re-search-forward
         (concat
          ;; we are probably looking at some interface declaration, skip
          "\\("
          "^.*void.*setsprite(.*;$"
          "\\)"
          "\\|"
          "\\("
          ;;  5
          ;;  the invocation part of set sprite
          ;;  this is either an ommitted c, the name of some c,
          ;;  or the name of a sprite container
          "\\b\\(\\(?5:.+?\\)\\.\\)?"
          "setsprite"
          "("
          ;; 6
          ;; the method invocation argument list
          "\\(?6:.*\\)"
          ")"
          "\\)")
         nil
         t)
      (unless
          (or (match-string 1))
        (catch 'skip
          (replace-match
           (-->
            (save-match-data
              (team-csharp-parse-arg-list
               (match-string 6)))
            (concat
             ;; contexts part
             (if
                 (and
                  (match-string 4)
                  (not (= (length it) 2)))
                 (match-string 4)
               "c.")
             "LoadSpriteAsync"
             "("
             (pcase it
               ;; single arg, this is the method that directly sets the sprite
               ;; we are not looking for that
               (`(,name) (throw 'skip t))
               ;; this is the default method we are looking for
               (`(,building ,container ,sprite-name ,image-name)
                (team/comma-interposed
                 (save-match-data
                   (resolve-sprite-loader-name
                    container))
                 sprite-name
                 image-name))
               ;; invocation on a sprite container
               (`(,sprite-name ,image-name)
                (team/comma-interposed
                 (save-match-data
                   (resolve-sprite-loader-name
                    (-last-item
                     (s-split
                      "\\."
                      (-->
                       (match-string 5)
                       ;;  this is the version that sets some address directly,
                       ;;  we do not care about that
                       (if (string-equal "c" it)
                           (throw 'skip t)
                         it))))))
                 sprite-name
                 image-name))
               (_ (throw 'skip t))
               ;; (_ (error "Dont know how to handle %d args of sprite container" (length it)))
               )
             ")")))
          (setq dirty t))))
    dirty))


(defun replace-sprite-loader-syntax-cmd ()
  (interactive)
  (team/with-default-dir
   idlegame-assets-dir
   (let ((cos-investigated-file
          (buffer-file-name)))
     (replace-sprite-loader-syntax))))




(defun cos/re-replace (file-re re replace &optional exclude-files)
  "Search .cs files with FILE-RE in cos project using rg,
then replace RE with REPLACE in each file."
  (team/with-default-dir
   idlegame-assets-dir
   (--each
       (let ((files
              (cos/cs-files-with-matches
               file-re)))
         (if exclude-files
             (-difference
              files
              exclude-files)
           files))
     (team/check-file
      it
      (while
          (re-search-forward re nil t)
        (replace-match replace)
        (setq team/check-file-dirty t))))))


(defun make-sprite-invocations-one-line ()
  (interactive)
  (while
      (re-search-forward
       "\\(void SetSprite\\)\\|\\(SetSprite(.*=>\\)\\|\\(\\bSetSprite(\\)" nil t)
    (when
        (match-string 3)
      (chsarp-put-statement-one-line))))

;;;###autoload
(defun sprite-loader-replace-syntax-do-it ()
  "Search cos for set sprite syntax,
attempt to replace with set sprite async syntax."
  (interactive)
  (team/with-default-dir
   idlegame-assets-dir
   (cos-investigate-files
    (-difference
     (cos/cs-files-with-matches
      "SetSprite\\(.*,")
     spriteloaders-skip-files)
    (make-sprite-invocations-one-line)
    (->gg)
    (replace-sprite-loader-syntax))))

(defun team/magit-commit-unstaged ((msg))
  "Run git sync. Make a commit with all changed tracked files and message MSG."
  (magit-run-git "add" "-u")
  (magit-run-git "commit" "-m" msg))


;; misc

(defun sprite-container-add-obs-attr ()
  (interactive)
  (->gg)
  (while
      (re-search-forward "public void SetSprit" nil t)
    (save-excursion
      (forward-line -1)
      (team/in-new-line
       "[Obsolete(\"SpriteContainers are obsolete, use LoadSpriteAsync instead\")]"))))





(defun sprite-loader-refactor-do-it ()
  (team/with-default-dir
   idlegame-assets-dir
   (sprite-loader-replace-syntax-do-it)
   ;; (team/magit-commit-unstaged "Replace sprite loader syntax")
   (cos/re-replace
    "LoadSpriteBlocked\("
    "LoadSpriteBlocked("
    "LoadSpriteAsync("
    spriteloaders-skip-files)))





;;; centaur code

(defun dwim-sprite-container-to-loader ()
  (interactive)
  (when
      (team/re-this-line "SetSprite(" t)
    (insert
     (-->
      (my/region-or-line-bounds)
      (prog1
          (buffer-substring-no-properties
           (car it)
           (cdr it))
        (delete-region (car it) (cdr it)))
      (with-temp-buffer
        (insert it)
        (->gg)
        (replace-sprite-loader-syntax)
        (buffer-string)))))
  (my/with-dwim-region
   (while (re-search-forward "SpriteContainer\.\\(\\w+\\)" end t)
     (replace-match
      (save-match-data
        (resolve-sprite-loader-name
         (match-string 1))))))
  (my/with-dwim-region
   (while (re-search-forward "\\bstring\\b" end t)
     (replace-match "LoaderName")))
  (my/with-dwim-region
   (while (re-search-forward "Container" end t)
     (replace-match "Loader"))))


(defun try-kill-sprite-loader-name ()
  (interactive)
  (let ((cos-investigated-file (buffer-file-name)))
    (print (resolve-sprite-loader-name
            (thing-at-point 'word)))
    ;; (kill-new
    ;;  (or (resolve-sprite-loader-name
    ;;       (thing-at-point 'word))
    ;;      (error "Did not find any sprite loader name for %s"
    ;;             (thing-at-point 'word))))
    ))
