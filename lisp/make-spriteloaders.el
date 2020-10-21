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

(defun sprite-container--names ()
  (team/with-file
   sprite-container-names-enum-file
   (print "expensive work")
   (team/collect--reg
    "public const string \\(\\w+\\)")))

(defun sprite-container-names ()
  (unless
      (bound-and-true-p sprite-container--names)
    (setq sprite-container--names
          (team/memoize-simple
           #'sprite-container--names)))
  (funcall sprite-container--names))




(cos-asset-type "/home/benj/idlegame/IdleGame/Assets/LoadGroups/Roulette/Roulette.prefab")

(cl-defstruct
    unity-asset-usage
  (prefab-path
   script-guid
   script-path
   game-object-name
   asset-type))


(defun team-unity/script-usages (file-or-meta))

(defconst yml-terminator "^--- ")

(team-unity/detailed-asset-usage "f")

(defun team-unity/detailed-asset-usage (file-or-meta)
  "Return a list `unity-asset-usage' for FILE-OR-META."

  (let ((file-or-meta (sprite-container-asset-file "CommunityBossCardsSprites")))
    (team/with-default-dir
     idlegame-assets-dir
     (--map
      (let ((items '()))
        (team/with-file
         "/home/benj/idlegame/IdleGame/Assets/PrefabBase.prefab"
         ;; it
         (while
             (re-search-forward
              (format
               (concat
                ;;  1
                ;; inside some arr
                "\\("
                "^  - {fileID: [0-9]+, guid: %1$s,"
                "\\)"
                ;;  2, 3
                ;; assigned to a field
                "\\|"
                "\\(\\w+\\): {fileId: [0-9]+, guid: %1$s"
                "\\)"
                ;;  4
                ;; as override
                "\\|"
                "\\("
                "^      objectReference: {fileId: [[:alnum:]]+, guid: %1$s,"
                "\\)")
               (team-unity/file-guid
                file-or-meta))
              nil t)
           (when (match-string 4)
             ;;  go upwarts to the transform modification the guid is the prefab guid
             (error "Encountered an override reference, this is not supported yet"))
           (let ((field-name (match-string 3)))
             (save-excursion
               (re-search-backward
                yml-terminator)
               (unless
                   (re-search-forward
                    "^  m_Script: {fileId: [[:alnum:]]+, guid: \\(\\w+\\), type: 3}"
                    (save-excursion
                      (re-search-forward yml-terminator nil t))
                    t)
                 (error "Did not find a script guid inside game object.\n Prefab: %s\n guid: %s"
                        it (team-unity/file-guid file-or-meta)))
               (let* ((script-guid (match-string 1))
                      (script-file-path (team-unity/asset-path-from-guid script-guid)))

                 (re-search-backward "^GameObject:$")
                 (re-search-forward
                  "^  m_Name: \\(\\w+\\)$" (save-excursion
                                             (re-search-forward yml-terminator nil t)))
                 (push
                  (make-unity-asset-usage
                   :prefab-path it
                   :script-guid script-guid
                   :script-path script-file-path
                   :game-object-name (match-string 1)
                   :asset-type (cos/asset-type it))
                  items))))))
        items)
      (--filter
       (s-ends-with-p ".prefab" it)
       (asset-usages file-or-meta)))))
  )


(--map

 ;; list of (script-name prefab game-object) where used

;;; map the usages


 (if online
     (team/append-line-to-file
      online-loader-names
      loader-name)
   (team/append-line-to-file
    offline-loader-names
    loader-name)

   )
 ;; determine online offline
 ;; check each load group entry in the usages ; check labels
 ;; if all are online, this sprites container is online
 ;; add to a file

 (sprite-container-names)

 )

(defun sprite-container-asset-file (name)
  (id-when
   (concat idlegame-assets-dir "#/Scripts/View/ScriptableObjects/" name ".asset.meta")
   #'file-exists-p))

(team/def-memoized asset--usages (guid)
  "Return a list of file names where GUID is used."
  (--mapcat
   (-->
    (concat idlegame-assets-dir it)
    (team/with-default-dir
     it
     (ignore-errors
       (mapcar
        #'(lambda (elm)
            (and elm (concat (file-name-as-directory it) elm)))
        (apply
         #'process-lines
         (team-unity/rg-guid--search-args
          guid)))))
    (delq nil it)
    (-map #'file-relative-name it))
   '("#" "Prefabs" "LoadGroups")))

(team/def-memoized asset-usages (file)
  "Return a list of file names where FILE is used."
  (--remove (s-ends-with-p ".meta" it)
            (asset--usages (team-unity/file-guid file))))


(defun team-unity/asset-path-from-guid (guid)
  (--first
   (s-ends-with-p ".meta" it)
   (asset--usages guid)))




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

(team-unity/asset-name-from-guid
 (team-unity/file-guid (sprite-container-asset-file "CommunityBossCardsSprites")))

(asset--usages
 (team-unity/file-guid (sprite-container-asset-file "CommunityBossCardsSprites")))
(asset-usages (sprite-container-asset-file "CommunityBossCardsSprites"))
