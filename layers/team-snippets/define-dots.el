(defconst cos/dot-prestine-commit "cb5747eb8e204a653c8f7cf46cb3e2e587102409")
(defconst cos/dot-source-file (concat cos-dir "/IdleGame/Assets/#/Sources/GameGuide/DotSource.cs"))
;; (defconst cos/dot-ext (concat cos-dir "/IdleGame/Assets/#/Sources/GameGuide/DotSourceDailyRefreshExt.cs"))

(defvar cos/dots-definitions '())
(file-exists-p cos/dot-source-file)

(defun cos/define-dot (name &key re-add-on-daily)
  (team/with-file
   cos/dot-source-file
   (unless (re-search-forward name nil t)
     (csharp-mode)
     (re-search-forward "//Red dots have offset of 10.000" nil)
     (forward-line -2)
     (team/in-new-line (format "%s," name))
     (when re-add-on-daily
       (re-search-forward "ReaddOnDailyRefresh" nil t)
       (forward-line 1)
       (->$)
       (team/in-new-line (format "case DotSource.%s:" name))))))

(defun cos/revert-dots-definitions ()
  (team/with-default-dir
   cos-dir
   (magit-file-checkout cos/dot-prestine-commit cos/dot-source-file)))

(defun cos/generate-dots ()
  (interactive)
  (cos/revert-dots-definitions)
  (load "/home/benj/.spacemacs.d/layers/team-snippets/dots-definitions.el"))


(defun cos/define-red-dots (dots)
  (team/with-file
   cos/dot-source-file
   (csharp-mode)
   (--map-indexed
    (unless (save-excursion (re-search-forward (car it) nil t))
      (->gg)
      (re-search-forward "//Quests" nil)
      (forward-line 1)
      (team/in-new-line
       (format "%s = RedDotOffset + %d," (car it) it-index))
      (when (and (cadr it) (not (save-excursion (re-search-forward (format "DotSource.%s" (car it)) nil t))))
        (re-search-forward "ReaddOnDailyRefresh" nil t)
        (forward-line 1)
        (->$)
        (team/in-new-line (format "case DotSource.%s:" (car it)))))
    dots)))

(defun cos/put-quest-dots ()
  (team/with-file
   cos/dot-source-file
   (--map-indexed
    (progn
      (->gg)
      (re-search-forward "//Quests" nil)
      (skip-chars-forward "^}")
      (forward-line -2)
      (team/in-new-line
       (format "%s = QuestDotsOffset + %d," it it-index))
      (re-search-forward "ReaddOnDailyRefresh" nil t)
      (forward-line 1)
      (->$)
      (team/in-new-line (format "case DotSource.%s:" it)))
    (cos/quest-dot-sources)))
  )

(team/with-file
 cos/dot-source-file
 (--map-indexed
  (progn
    (->gg)
    (re-search-forward "FromQuestSourceToMenu")
    (forward-line 1)
    (->$)
    (team/in-new-line (format "DotSource.%s => MenuType.%s," (concat "Quest" it) it)))
  (team/file-lines "menu-names")
  ))

(team/with-file
 cos/dot-source-file
 (--map-indexed
  (progn
    (->gg)
    (re-search-forward "FromMenuToQuestSource")
    (forward-line 1)
    (->$)
    (team/in-new-line (format "MenuType.%s => DotSource.%s," it (concat "Quest" it))))
  (team/file-lines "menu-names")
  ))

(defun cos/put-red-dot-systems (dots)
  (--map (cos/make-dot-system (car it)) dots))

;; (team)

(defun cos/quest-dot-sources ()
  (--map (concat "Quest" it) (team/file-lines "menu-names")))


(defun cos/put-presents-dots ()
  (team/with-file
   cos/dot-source-file
   (--map-indexed
    (progn
      (->gg)
      (re-search-forward "// presents" nil)
      (forward-line -1)
      (team/in-new-line
       (format "%s = PresentsOffset + %d," it it-index))
      (re-search-forward "ReaddOnDailyRefresh" nil t)
      (forward-line 1)
      (->$)
      (team/in-new-line (format "case DotSource.%s:" it)))
    (--map (concat it "Presents") (append (team/file-lines cos/purchase-menus) (team/file-lines cos/purchase-overlays)))
    ))
  )

(defun cos/put-presents-enum ()
  (team/with-file
   cos/dot-source-file
   (--map-indexed
    (progn
      (->gg)
      (re-search-forward "return building.Menu switch" nil)
      (team/in-new-line (format "MenuType.%s => DotSource.%s," it (concat it "Presents")))
      )
    (team/file-lines cos/purchase-menus)
    ))
  )

(team/with-file
 cos/dot-source-file
 (--map-indexed
  (progn
    (->gg)
    (re-search-forward "return building.Overlay switch" nil)
    (team/in-new-line (format "OverlayType.%s => DotSource.%s," it (concat it "Presents")))
    )
  (team/file-lines cos/purchase-overlays)
  ))

(concat "fa" "fue")
(--map (concat it "Presents") (subseq  (append (team/file-lines cos/purchase-menus) (team/file-lines cos/purchase-overlays)) 0 2 ))
(--mapcat)


(count-lines )
(defvar cos/purchase-overlays "purchase-overlays")
(defvar cos/purchase-menus "purchase-menus")

(+ (team/file-line-count cos/purchase-overlays)
   (team/file-line-count cos/purchase-menus))




(defun team/file-line-count (file)
  (team/with-file
   file
   (count-lines (point-min) (point-max))))

(cos/put-red-dot-systems cos/red-dots)

(cos/define-red-dots cos/red-dots)

(cos/make-dot-system "InpectionLootcard")
(cos/make-dot-system "InspectionLootcardClaim")
(load "/home/benj/.spacemacs.d/layers/team-snippets/red-dots")

(team/with-file
 "/home/benj/idlegame/IdleGame/Assets/#/Sources/Purchase/PurchaseConstantsGenerated.cs"
 (team/while-reg
  "building : \\(\\w+\\)\.\\(\\w+\\),"
  (let ((type (match-string-no-properties 1))
        (name (match-string-no-properties 2)))
    (team/append-line-to-file
     name
     (pcase type
       ("MenuType" "purchase-menus")
       ("OverlayType" "purchase-overlays"))))))





(with-temp-buffer
  (insert "foo")
  (open-line 1)
  (append-to-file
   nil
   nil
   "my-file"
  ))





(provide 'define-dots)
