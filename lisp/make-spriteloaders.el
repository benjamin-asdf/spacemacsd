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








(defun resolve-loader-name (s)
  )











;;;  assume that each script


;;;  issue you have a field as sprites container

;;;  you want to put a loader now












































(defun fill-loaders ()

  (--map




   (sprite-container-names)
   )
  )
