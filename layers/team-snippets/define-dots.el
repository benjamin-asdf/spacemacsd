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
    (team/file-lines cos/purchase-menus))))

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



(cos/define-dot "CampaignDotmanRefresh" nil t)
(cos/define-dot "ArenaDotmanRefresh" nil t)





(defun extract-quest-buildings ()
  "Return a list of elements like (type name) where type is ther MenuType or OverlayType."
  (team/with-default-dir
   idlegame-project-root
   (team/with-file
    "Assets/#/Sources/Quests/QuestConstants.cs"

    )
   )

  )





(defun read-csharp-field-value ()
  "If csharp syntax at point is something like arr = FrozenArr(stuff1,stuff2), evaluate to a list of strings like (stuff1,stuff2)"
  (->$)
  (--map (string-trim it)
         (split-string
          (string-trim
           (buffer-substring
            (save-excursion (forward-char 1) (point))
            (save-excursion (skip-chars-forward "^)") (point)))) ",")))

(defun cos/leaderboard-deals ()
  (team/with-file
  "/home/benj/idlegame/IdleGame/Assets/#/Sources/Leaderboards/Shared/LeaderboardsConstants.cs"
  (re-search-forward "ALL_LEADERBOARD_DEALS_PRODUCT_IDS =>")
  (read-csharp-field-value)))

(defun cos/leaderboard-products-rec ()
  (team/with-file
  "/home/benj/idlegame/IdleGame/Assets/#/Sources/Leaderboards/Shared/LeaderboardsConstants.cs"
   (--map
    (if-let  ((s
               (with-temp-buffer
                 (print it)
                 (insert it)
                 (->gg)
                 (when (re-search-forward ".*\\(PRODUCT_ID.*DEALS\\)")
                   (match-string-no-properties 1)))))
        (progn (print s)
               (->gg)
               (re-search-forward (format "%s => " s))
               (read-csharp-field-value))
      (team/mklist it))
    (team/mklist (car (cos/leaderboard-deals))))))

(cos/leaderboard-products-rec)


(defun cos/product-data (product-id)
  "Get the chsarp string line defining the product of PRODUCT-ID."
  (team/with-file
   "/home/benj/idlegame/IdleGame/Assets/#/Sources/Purchase/PurchaseConstantsGenerated.cs"


   ))

(let ((s "foobar"))
  (print (string-match "oo\\(.*\\)" s))
  (match-string-no-properties 0))

(defun team/map-first-lines-with-match (file strings)
  "Return a new list of file lines. Search FILE for each first occurance of string in STRINGS."
  (team/with-file
   file
   (--map
    (progn
      (->gg)
      (re-search-forward it)
      (buffer-substring (point-at-bol) (point-at-eol)))
    strings)))



(team/map-first-lines-with-match
 "/home/benj/idlegame/IdleGame/Assets/#/Sources/Purchase/PurchaseConstantsGenerated.cs"
 (team/mklist (car (cos/leaderboard-products)))
 )

(defun team/read-chsarp-map (str)
  (let ((res))
    (with-temp-buffer
      (insert str)
      (->gg)
      (while
          (re-search-forward "(\\(.+?\\),.+?\\(.+?\\))" nil t)
        (push `((,(match-string 1) ,(match-string 2))) res)))
    res))

(defvar
  overlay-quest-dots
  (team/with-file
  "/home/benj/.spacemacs.d/layers/team-snippets/quest-overlay-map"
  (team/read-chsarp-map (buffer-string))
  ))


(team/with-file
 "/home/benj/idlegame/IdleGame/Assets/#/Sources/GameGuide/DotSource.cs"
 (forward-line 1191)
 (let ((m (point-marker)))
   ;; (--map-indexed
   ;; (-->
   ;; (cadr (s-split "\\." (caar it) t))
   ;; (team/in-new-line
   ;; (format "Quest%s = GreenQuestDotOffset + %d," it it-index)))
   ;; overlay-quest-dots)

   (--map-indexed
    (team/in-new-line
     (format "%s => %s,"
             (cadar it)
             (concat "DotSource.Quest" (cadr (s-split "\\." (caar it) t)))))
    overlay-quest-dots
    )
   (csharp-mode)
   (indent-region-line-by-line m (point))
   )
 )


(cadr (s-split "\\." (caar (car overlay-quest-dots)) t))
(cadr (s-split "\\." (caar (car overlay-quest-dots)) t))
(cadar (car overlay-quest-dots))



;;   put lb dots
(--map
 (team/with-file
  cos/dot-source-file
  (csharp-mode)
  (let ((name (concat "LbSubReminder" it)))
    (unless (re-search-forward name nil t)
      (->gg)
      (re-search-forward "//Quests" nil)
      (forward-line -1)
      (team/in-new-line (format "%s," name)))

    (re-search-forward "FromLbToLBReminderSource")
    (forward-line 2)

    (team/in-new-line (format "LB.%s => DotSource.%s," it name))

    )

  )
 (team/file-lines "/tmp/lb-names")
 )


(team/with-file
 cos/dot-source-file
 (csharp-mode)
 (--map
  (let ((name (concat "Quest" it)))
    (->gg)
    (unless (re-search-forward name nil t)
      (re-search-forward "//Quests in menus" nil)
      (forward-line 1)
      (team/in-new-line (format "%s," name))
      (re-search-forward "FromQuestTypeToDotSource")
      (forward-line 2)
      (team/in-new-line (format "QuestType.%s => DotSource.%s," it name))))
  menu-quest-types))

(team/with-file
 cos/dot-source-file
 (csharp-mode)
 (--map-indexed
  (let* ((type-name (cadr (s-split "\\." (caar it) t)))
        (name (concat "Quest" type-name)))
    (->gg)
    (unless (re-search-forward name nil t)
      (re-search-forward "// overlay quests" nil)
      (re-search-forward "^$")
      (team/in-new-line (format "%s = RedQuestDotOffset + %d," name it-index))
      (re-search-forward "// overlay quest dots")
      (team/in-new-line (format "QuestType.%s => DotSource.%s," type-name name))))
  overlay-quest-dots
  ))

(caar (car overlay-quest-dots))


;; get all menu quest types
;; define dot sources for quest type

;; switch (questType)
;; => DotSource.

(defvar
  menu-quest-types
  (team/with-file
  "/home/benj/.spacemacs.d/layers/team-snippets/menu-quest-lookup"

  (team/collect--reg
   "\\(QuestType\.\\)?\\(\\w+\\)"
   2
   )

  ))







(provide 'define-dots)
