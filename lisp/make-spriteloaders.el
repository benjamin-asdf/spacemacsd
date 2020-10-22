;; -*- lexical-binding: t; -*-

(require 'enums)
(require 'unity-labels)

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




(defun online-loader-p (container)
  (--every?
   (eq 'online
       (unity-asset-usage-asset-type it))
   (-flatten
    (team-unity/detailed-asset-usage
     (or
      (sprite-container-asset-file container)
      (error "%s does not have an asset file" container))))))





;; (--map

;;  ;; list of (script-name prefab game-object) where used

;; ;;; map the usages


;;  (if online
;;      (team/append-line-to-file
;;       online-loader-names
;;       loader-name)
;;    (team/append-line-to-file
;;     offline-loader-names
;;     loader-name)

;;    )
;;  ;; determine online offline
;;  ;; check each load group entry in the usages ; check labels
;;  ;; if all are online, this sprites container is online
;;  ;; add to a file

;;  (sprite-container-names)

;;  )

(defun sprite-container-asset-file (name)
  (id-when
   (concat idlegame-assets-dir "#/Scripts/View/ScriptableObjects/" name ".asset.meta")
   #'file-exists-p))


(team/with-file
 "/home/benj/idlegame/LoadingIdlegame/Packages/LoadingIdlegame/Runtime/LoaderName.cs"
 (csharp-mode)
 (re-search-forward "OnlineLoaderName")
 (forward-line)
 (team-csharp/append-to-enum "foo")

 )


(defun find-missing-loaders ()
  "Return 1) the loader names that are implied by the presence of a SpritesContainer,
but are missing in the loader enums.
2) A list of loaders that we did no map to a sprite container."



  )






(defvar
  example-set-sprite
  "injectedC.SetSprite(MenuType.Church, SpriteContainer.HolyBookSprites, \"AthenePack_\" + type, m.athenePackView.athenePackBackground);")

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


    )

  )


(ert-deftest replace-syntax-set-sprite-4-args ()
  (with-temp-buffer
    (insert example-set-sprite)
    (->gg)


    )


  )

;; (team-unity/asset-name-from-guid
;;  (team-unity/file-guid (sprite-container-asset-file "CommunityBossCardsSprites")))

;; (asset--usages
;;  (team-unity/file-guid (sprite-container-asset-file "CommunityBossCardsSprites")))
;; (asset-usages (sprite-container-asset-file "CommunityBossCardsSprites"))

(defun resolve-sprite-loader-name (container))

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
      (if (online-loader-p
           it)
          online-loader-names
        offline-loader-names)
      it))
   (sprite-container-names)))



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
      (if (online-loader-p
           it)
          online-loader-names
        offline-loader-names)
      it))
   (sprite-container-names)))


(defun add-loader-names-from-files ()
  "Check `offline-loader-names' and `online-loader-names'.
Put enum syntax into `loader-name-file'."
  (flet ((add-loader (type name)
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
         offline-loader-names))))))



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
    (re-search-forward "\\(\\w+\\)\.setsprite(.+?,\\(.+?\\),\\(.+?\\))")
    (replace-match
     (format "\\1.LoadSpriteAsync(%s,\\3)"
             (resolve-loader-name
              (match-string 2))))
    (buffer-string)))

(defun dump--replace-sprite-loader-syntax (s)
  (re-search-forward "\\(\\w+\\)\.setsprite(.+?,\\(.+?\\),\\(.+?\\))")
  (replace-match
   (format "\\1.LoadSpriteAsync(%s,\\3)"
           (resolve-loader-name
            (match-string 2))))
  (buffer-string))





