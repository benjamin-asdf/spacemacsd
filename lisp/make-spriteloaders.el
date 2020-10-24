;; -*- lexical-binding: t; -*-

(require 'enums)
(require 'unity-labels)
(require 'unity-asset-usages)



(cl-defun mk-hash-from-list (list &rest args)
  (let ((res
         (eval `(make-hash-table ,@(or args nil)))))
    (dolist (elm list)
      (puthash elm t res))
    res))



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


;;  add to the enum

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
  (id-when
   (concat idlegame-assets-dir "#/Scripts/View/ScriptableObjects/" name ".asset.meta")
   #'file-exists-p))



(defun parse-set-sprite-args (s)
  "Evaluate to a plist with keys
:building
:sprite-container
:sprite-name
:image "
  (with-temp-buffer
    (insert s)
    (->gg)
    (unless (re-search-forward "setsprite(\\(.*\\))" nil t)
      (error "Did not find set sprite in %s" s))
    (-interleave
     '(:building :sprite-container :sprite-name :image)
     (-map #'s-trim (split-string
       (match-string-no-properties 1) ",")))))

(defun replace-syntax-i ()
  (interactive)
  (replace-syntax))

(defun replace-syntax ()
  "Replace set sprite with load sprite async syntax in buff."
  (->gg)
  (while (re-search-forward
          "\\(\\w+\\)\.setsprite")
    ))






(defun resolve-sprite-loader-name-with-container-name (container)
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


(defun set-missing-sprite-containers ()
  (-map
   #'team/delete-file-when-exitst
   (list online-loader-names offline-loader-names))
  (--map
   (unless
       (resolve-sprite-loader-name-with-container-name it)
     (team/append-new-line
      (if (container-online--p
           it)
          online-loader-names
        offline-loader-names)
      it))
   (sprite-container-names)))



(defvar-local cos-investigated-file nil)

(defmacro cos-investigate-file (file &rest body)
  `(team/with-file
    file
    (setq cos-investigated-file file)
    ,@body))

(defun resolve-sprite-loader-field (field-name &optional file-name)
  "If FILE-NAME or buffer file is a script that has some SpriteContainer field called FIELD-NAME.
Search the first prefab with the scritp for a sprite container ref,
return the sprite container name."
  (or
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
    (concat
     (if (loader-name--online-loader-p it)
         "OnlineLoaderName."
       "LoaderName.")
     it)))

(defun from-sprite-container-file-to-loader-name (s)
  (-some--> s
    (or (and (file-exists-p it) it)
        (error "No such sprite container file %s" it))
    (file-name-base (file-name-base it))
    (or
     (resolve-sprite-loader-name-with-container-name it)
     (error "There was no loader name for %s" it))))


;;;  issue you have a field as sprites container

;;;  you want to put a loader now




(defun set-missing-sprite-containers ()
  "Put files with at `offline-loader-names' and `online-loader-names'.
These are all sprite containers that did not have a corresponding loader in the loader name.
Takes 60s if not initialized."
  (-map
   #'team/delete-file-when-exitst
   (list online-loader-names offline-loader-names))
  (--map
   (unless
       (resolve-sprite-loader-name it)
     (team/append-new-line
      (if (container-online--p
           it)
          online-loader-names
        offline-loader-names)
      it))
   (sprite-container-names)))


(defun add-loader-names-from-files ()
  "Check `offline-loader-names' and `online-loader-names'.
Put enum syntax into `loader-name-file'.
As side effect reset `lookup-loaders' so it has the updated data next time it is called."
  (cl-flet ((add-loader (type name)
                     (setq name (concat name "Loader"))
                     (->gg)
                     (re-search-forward type)
                     (team-csharp-prepend-at-curly
                      (concat name ",") 4)
                     (re-search-forward (format "this %s" type))
                     (team/prepend-at-re
                      "case"
                      (format
                       "case %1$s.%2$s: return nameof(%1$s.%2$s);" type name)
                      12)))
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
  (set-missing-sprite-containers)
  (add-loader-names-from-files))



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
  (while
      (re-search-forward
       "\\(\\w+\\)\.setsprite(.+?,\\(.+?\\),\\(.+?\\))")
    (replace-match
     (format "\\1.LoadSpriteAsync(%s,\\3)"
             (save-match-data
               (resolve-sprite-loader-name
                (match-string 2)))))))


(defun dump--replace-sprite-container-invocation ()
  (while
      (re-search-forward
       "\\(\\w+\\)\.SetSprite(\\(.+?\\),\\(.+?\\))" nil t)
    (replace-match
     (format
      "c.LoadSpriteAsync(%s,\\3)"
      (save-match-data
        (resolve-sprite-loader-name
         (match-string 2)))))))





(defun cos/cs-fiels-with-matches (re)
  "See `files-with-matches'."
  (team/with-default-dir
   idlegame-assets-dir
   (--filter
    (string-match-p "\.cs$" it)
    (files-with-matches))))

(defun files-with-matches (re)
  "Return a list of files containing RE, use rg."
  (my-process-lines
   "rg"
   nil
   (current-buffer)
   nil
   "-l"
   re))




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
