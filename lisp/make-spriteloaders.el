;; -*- lexical-binding: t; -*-

(require 'enums)
(require 'unity-labels)
(require 'unity-asset-usages "/home/benj/.spacemacs.d/lisp/unity-asset-usages.el")
(require 'idlegame-definitions)

(cl-defun mk-hash-from-list (list &rest args)
  (let ((res
         (eval `(make-hash-table ,@(or args nil)))))
    (dolist (elm list)
      (puthash elm t res))
    res))

(defvar-local cos-investigated-file nil)

(defmacro cos-investigate-file (file &rest body)
  `(team/with-file
    file
    (setq cos-investigated-file file)
    ,@body))



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



(defconst online-loader-names (concat cos-dir "/LoadingIdlegame/Packages/LoadingIdlegame/Runtime/online-loader-names"))
(defconst offline-loader-names (concat cos-dir "/LoadingIdlegame/Packages/LoadingIdlegame/Runtime/offline-loader-names"))

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
   (--mapcat
    (list (concat it "Loader"))
    (list
     (team/re-replace-in-string
      container
      "Sprites"
      "sSprites"
      t)
     container))))



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
  (-some-->
      (or
       (resolve-sprite-loader-name-with-container-name
        (or
         (and
          (string-match "SpriteContainer\\.\\(\\w+\\)" container)
          (match-string 1 container))
         (and
          cos-investigated-file
          (resolve-sprite-loader-field
           container))
         container))
       "TODO")
    (concat
     (if (loader-name--online-loader-p it)
         "OnlineLoaderName."
       "LoaderName.")
     it)))


(resolve-sprite-loader-name-with-container-name
 "InsideGarrisonSprites.asset.meta")



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
  (sprite-loaders-refresh-names)
  (add-loader-names-from-files))

(team/with-file
 "/home/benj/idlegame/LoadingIdlegame/Packages/LoadingIdlegame/Runtime/online-loader-names"
 (-map
  #'file-name-base


  )
 )



(defconst
  spriteloaders-skip-files '("ResourceManagementExtentions"))

;;;  the missing thing is to write all sprite container dirs to some file
;;;  (online, offline) file that already exists
;;;  then generate the loaders with unity

(defun dump-replace-sprite-loader-syntax (s)
  (with-temp-buffer
    (insert s)
    (->gg)
    (dump--replace-sprite-loader-syntax)
    (buffer-substring)))

(defun dump--replace-sprite-loader-syntax ()
  (let ((res))
    (while
        (re-search-forward
         ;; call set sprite with contexts,
         ;; c.  can be omitted, if we are inside an extension class
         (concat
          "\\(\\w+\\.\\)?"
          "setsprite(.+?,\\(.+?\\),\\(.+?\\))")
         nil t)
      (unless
          (save-match-data
            (re-search-forward "{" (point-at-eol) t))
        (replace-match
         (format "\\1LoadSpriteAsync(%s,\\3)"
                 (save-match-data
                   (resolve-sprite-loader-name
                    (match-string 2))))
         (setq res t))))
    t))

(defun dump--replace-sprite-container-invocation ()
  (let ((res))
    (while
        (re-search-forward
         "\\(\\w+\\)\\.SetSprite(\\(.+?\\),\\(.+?\\))" nil t)
      (replace-match
       (format
        "c.LoadSpriteAsync(%s,\\3)"
        (save-match-data
          (resolve-sprite-loader-name
           (match-string 1))))
       (setq res t)))
    res))



;;; centaur code

(defun dwim-sprite-container-to-loader ()
  (interactive)
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
