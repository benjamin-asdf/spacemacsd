(defconst move-achv-code-getter-template
  "
        void _updateView() {
            if (!c.TryGetAchvProgress%sView(AchvViewId.%s, out var view)) return;
%s
        }
"
  )


(defconst achv-file "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/AchievementProgressDisplaySystems.cs")


(defun benj-add-update-view-code ()
  "Add update view syntax to all systems in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "public \\w+System(Contexts c) : base(c) {" nil t)
    (forward-line 1)
    (open-line 1)
    (insert
"
        AddReact(Matcher.AllOf<AchvProgressViewC>(), e => {
            _updateView();
        });
"

     )))

(defun benj-delete-achv-scripts ()
  "Delete icon and bar scripts from the dirs. Keep special"

  (interactive)
  (dolist (dir dirs-with-monos)
      (dolist (file (benj-directory-files dir))
        (unless (string-equal "meta" (file-name-extension file))
          (with-temp-file file
            (insert-file-contents-literally file)
            (if (search-forward "// special" nil t)
                (progn (re-search-backward "Obsolete")
                       (goto-char (point-at-bol))
                       (kill-line t)
                       (goto-char (point-at-bol))
                       (kill-line t))

              (progn (delete-file file) (delete-file (concat file ".meta"))))

  )))))

(defun benj-move-achv-code (&optional file)
  "Reformat update view code to fit new api."
  (interactive"fFile to rewrite or default: ")
  (setq file (or file achv-file))
  (with-temp-file file
    (insert-file-contents-literally file)
    (benj-remove-eol-in-buff)
    (let ((text) (min) (max) (func-spot) (achvname) (kindname))
      (while (re-search-forward "AddReactEach(Matcher\.AllOf<\\(\\w+\\)C.*UpdateView>(), e => {" nil t)
        (setq achvname (match-string 1))
        (setq kindname (if (string-match "Bar" achvname) "Bar" "Icon"))
        (save-excursion
          (setq func-spot (copy-marker (point-at-eol)))
          (evil-end-of-line)
          (save-excursion
            (forward-line 1)
            (setq min (copy-marker (point-at-bol))))
          (evil-jump-item)
          (save-excursion
            (forward-line -1)
            (setq max (copy-marker (point-at-eol)))))
        (setq text (buffer-substring min max))
        (kill-region (save-excursion
                       (goto-char min)
                       (search-backward "AddReactEach")
                       (point-at-bol))
                     (save-excursion
                       (goto-char max)
                       (forward-line 1)
                       (point-at-eol)))

        (goto-char func-spot)
        (insert (format move-achv-code-getter-template kindname achvname text))))

    (goto-char (point-min))
    (while (re-search-forward "c.state.UpdateView<\\w+C>();" nil t)
      (replace-match "_updateView();"))

    (goto-char (point-min))
    (while (re-search-forward "^.*var view =.*Get<\\w+Progress\\w+>()\.value.*" nil t)
      (goto-char (point-at-bol))
      (kill-line t))
    (benj-add-update-view-code)))



(defconst dirs-with-monos '("/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/Icons/" "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/ProgressBars/"))


(defun benj-remove-eol-in-buff ()
  "Remove eol and aset to ut8."
  (interactive)
  (save-excursion (goto-char (point-min))
                  (while (re-search-forward "\r\n" nil t) (replace-match "\n"))))

(benj-achv-replace-obsolete-monos)



(defun benj-achv-replace-obsolete-monos ()
  "Replace syntax for acvh icons and bars, put obsolete attr."
  (interactive)
  (dolist (dir dirs-with-monos)
    (dolist (file (benj-directory-files dir))
      (unless (string-equal "meta" (file-name-extension file))
        (with-temp-file file
          (insert-file-contents-literally file)
          (unless (search-forward "[Obsolete]" nil t)
            (goto-char (point-min))
            (benj-remove-eol-in-buff)
            (set-buffer-file-coding-system 'no-conversion)
            (save-excursion (while (re-search-forward "\r\n" nil t) (replace-match "\n")))
            (goto-char (point-min))
            (if (not (search-forward "Component" nil t))
                (message "Did not find  Component in %s" file)
              (progn (forward-line -2)
                     (insert "using System;\n")
                     (forward-line 1)
                     (insert "\n[Obsolete]")
                     (cond ((re-search-forward "AchievementProgressBar<\\w+>" nil t)
                            (replace-match "AchvProgressBar"))
                           ((re-search-forward "AchievementProgressIcon<\\w+>" nil t)
                            (replace-match "AchvProgressIcon")))
                     (unless (search-forward "// special" nil t)
                       (goto-line 6)
                       (insert "[Obsolete]\n"))))))))))














(defconst bar-path "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/ProgressBars/")
(defconst icon-path "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/Icons/")
(defconst emote-icon-path "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/Icons/EmotesAchievementProgressIcon.cs")


(defvar bar-guids (benj-all-guids-at-path bar-path))
(defvar icon-guids (benj-all-guids-at-path icon-path))
(setq bar-guids (benj-all-guids-at-path bar-path))
(setq icon-guids (benj-all-guids-at-path icon-path))


(defconst new-bar-guid (benj-get-guid-with-meta
                        "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/AchvProgressBar.cs"
                        ))
(defconst new-icon-guid (benj-get-guid-with-meta "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/AchvProgressIcon.cs"))




(benj-achv-fix-prefabs (list "/home/benj/idlegame/IdleGame/Assets/Prefabs/ChurchPrefabInception/Holy Book.prefab"))



;; call this after merging,
;; prefabs to handle need to be the conflicted prefabs
(benj-achv-fix-prefabs prefabs-to-handle)

(mapcar (lambda (s) (replace-regexp-in-string "\\\\ " " " s)) prefabs-to-handle)

(defun benj-achv-fix-prefabs (prefabs)
  "Fix all the achv refs prefabs, PREFABS should be a list of strings
for the prefabs that need to be fixed."
  (dolist (prefab prefabs)
    (progn
      (with-temp-file prefab
        (insert-file-contents-literally prefab)
        (benj-achv--search-and-replace (regexp-opt bar-guids) "hello mofos!!")
        (benj-achv--search-and-replace (regexp-opt icon-guids) "hello mofos!! -icon")))))

(defun benj-achv--search-and-replace (str replstr)
  "Replace all occurances of STR with REPLSTR in buffer"
  (goto-char (point-min))
  (while (re-search-forward str nil t)
    (replace-match replstr)))



(setq presents-bar-file "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/ProgressBars/PresentsAchievementProgressBar.cs")

(setq presents-icon-file "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/ProgressBars/PresentsAchievementProgressIcon.cs")

(setq presents-bar-usages (benj-guid-file-usages (benj-get-guid-with-meta presents-bar-file)))

(setq presents-icon-usages (benj-guid-file-usages (benj-get-guid-with-meta presents-icon-file)))

(defun benj-achv-present-achv-display-type (prefab guid)
  "Search PREFAB for the display type of GUID.
GUID should be the guid of a present icon or prefab.
This evaluates to a number representing achvViewId enum if sucessful, nil otherwise."
  (with-temp-buffer
    (insert-file-contents-literally prefab)
    (when (and (re-search-forward guid nil t)
               (re-search-forward "^  displayType: \\([0-9]+\\)$" nil t))
      (string-to-number (match-string 1)))))


(defun benj-achv-present-display-types (present-usages present-guid)
  "List of preset bar display type usages.
PRESENT-USAGES should be a list of strings with paths where the present achviement is used.
PRESENT-GUID should be the guid of the present script."
  (mapcar
   (lambda (prefab)
     (and (string-match-p ".prefab$" prefab)
          (benj-achv-present-achv-display-type prefab guid)))
   present-usages))


(defun benj-achv-present-display-types-for-file (file)
  "File shoud be either `presents-bar-file' or `presents-icon-file'.
Evaluates to the display types used by this type as list of numbers."
  (let ((default-directory cos-dir)
        (guid (benj-get-guid-with-meta file)))
    (benj-achv-present-display-types (benj-guid-file-usages guid) guid)))


(defun achv-insert-presents-bar-types ()
  "Insert present bar types."
  (interactive)
  (achv-insert-display-types presents-bar-file))

(defun achv-insert-presents-icon-types ()
  "Insert present icon types."
  (interactive)
  (achv-insert-display-types presents-icon-file))

(defun achv-insert-display-types (file)
  "FILE should be either present bar or icon script file."
  (insert (mapconcat 'identity
                     (mapcar 'achv-display-type (benj-achv-present-display-types-for-file file))
                     "\n")))



(defun achv-display-type (num)
  "Achv id string from enum num, nil if out of bounds"
  (and num (nth-value num (split-string (replace-regexp-in-string "[ +\n]" "" achv-ids-string) ","))))




(setq achv-base-guids (mapcar 'benj-get-guid-with-meta achv-base-prefabs))


(defconst achv-base-prefabs '(
                              "IdleGame/Assets/Prefabs/Achievements/AchievementProgressBar.prefab"
                              "IdleGame/Assets/Prefabs/Achievements/AchievementBonusView.prefab"
                              "IdleGame/Assets/Prefabs/Achievements/AchievementBonusView_Crypto Variant.prefab"
                              "IdleGame/Assets/Prefabs/Achievements/AchievementBonusView_Crypto Variant.prefab"))



(setq guid (nth-value 1 achv-base-guids))

(defun achv-plus-one-to-all-acvh-view-ids ()
  "Loop over all changed prefabs and do +1 to all achv view ids."
  (let ((default-directory cos-dir))
    (dolist (prefab (seq-filter (lambda (p) (not (member p achv-base-prefabs))) (benj-all-changed-files nil nil "\\.prefab$")))
      (with-temp-file prefab
        (insert-file-contents-literally prefab)
        (while (re-search-forward "^  identifier: \\([0-9]+\\)$" nil t)
          (replace-match (format "  identifier: %s" (number-to-string (+ 1 (string-to-number (match-string 1)))))))))))



(defconst achv-ids
  (split-string (replace-regexp-in-string "[ +\n]" "" achv-ids-string) ","))



(defun benj-their-prefabs ()
  "Take all their prefabs, save result in repo root"
  )




(defun benj-magit-cmd (cmd)
  "Get current top dir using magit and run cmd as `async-shell-command'"
  (let ((default-directory (magit-toplevel)))
    (async-shell-command cmd)))


(let ((default-directory cos-dir))
  ;; (setq prefabs-to-handle (benj-unmerged-prefabs))
  (async-shell-command (format "git checkout develop -- %s" (mapconcat 'identity prefabs-to-handle " ")))
  )

(benj-magit-cmd
 )



;; (write-region () "unmerged-prefabs" )


;; (let ((default-directory (magit-toplevel)))
;;   (seq-filter (lambda (s) (string-match-p "prefab" s))
;;               (mapcar  (lambda (s) (replace-regexp-in-string "\w+ \w+ \w+ " "" s)) (split-string (shell-command-to-string "git ls-files -z --unmerged") (make-string 1 ?\0)))))


;; ("100644 340bd924786b98defba2d89399a35a6400fd7c01 1	Church.prefab" "100644 9ae1229c6dcd895cdd04e58be6f57e65198548ac 2	Church.prefab" "100644 e5f8d397b027101f9a32590381f66dadb0adeca3 3	Church.prefab" "")

;; (replace-regexp-in-string "\\w+ " "" "100644 340bd924786b98defba2d89399a35a6400fd7c01 1	Church.prefab")

;; (seq-filter (lambda (elt) (> elt 0)) [1 -1 3 -3 5])



(defconst achv-ids-string
                              "
    None,
    AdsAchievementProgressIcon,
    ArenaRefreshProgressIcon,
    AvatarLevelUpgradeProgressIcon,
    AvatarUpgradeChanceProgressIcon,
    BlockchainRentProgressIcon,
    CampaignStreamerPowerProgressIcon,
    CasinoRefreshProgressIcon,
    ChurchPrayProgressIcon,
    FaceSwapSkinBuffProgressIcon,
    FaceSwapSwapCostProgressIcon,
    FactionStarProgressIcon,
    FightingAtheneAuraProgressIcon,
    FusePetsAchievementProgressIcon,
    GeopetAttractProgressIcon,
    GeopetLuckProgressIcon,
    GoLiveExtraChestProgressIcon,
    HotStoriesLootPostProgressIcon,
    HotStoriesLootProfileProgressIcon,
    InspectionArmorBuffProgressIcon,
    InspectionAttackBuffProgressIcon,
    InspectionHealthBuffProgressIcon,
    InspectionLootCardProgressIcon,
    InspectionPowerBuffProgressIcon,
    MarketRefreshProgressIcon,
    MineExpeditionsProgressIcon,
    ReplayBookAchievementProgressIcon,
    MineSubProgressIcon,
    MonthlyCardProgressIcon,
    CommunityCardProgressIcon,
    ReplayBookReplayAchievementProgressIcon,
    ReplayBookHallOfFameAchievementProgressIcon,
    ShopExtraGemsProgressIcon,
    ShopSecondExtraGemsProgressIcon,
    SkinTradingExtraSlotsProgressIcon,
    SpellbookSwapCostProgressIcon,
    SummoningKappaProgressIcon,
    SummoningPogChampProgressIcon,
    SummoningTriHardProgressIcon,
    VoiceChatEmotePanelProgressIcon,
    StoriesEmotePanelProgressIcon,
    AchievementsAchievementProgressBar,
    FusionAchievementProgressBar,
    ArenaAchievementProgressBar,
    AttackStreamerAchievementProgressBar,
    AvatarAchievementProgressBar,
    BingoAchievementProgressBar,
    BlackjackAchievementProgressBar,
    BlockchainAchievementProgressBar,
    BossTowerAchievementProgressBar,
    CampaignAchievementProgressBar,
    CasinoAchievementProgressBar,
    ChestAchievementProgressBar,
    ChurchAchievementProgressBar,
    CommunityAchievementProgressBar,
    DailyEventAchievementProgressBar,
    JackpotAchievementProgressBar,
    MailAchievementProgressBar,
    MarketAchievementProgressBar,
    MerchantAchievementProgressBar,
    MineAchievementProgressBar,
    PokerAchievementProgressBar,
    QuestAchievementProgressBar,
    RouletteAchievementProgressBar,
    SkinTradingProgressBar,
    SpellBookAchievementProgressBar,
    SummoningAchievementProgressBar,
    TalentTreeAchievementProgressBar,
    WeeklyEventAchievementProgressBar,
    ClaimStreamerProgressIcon,
    TopUpItemAchievementProgressbar,
    AthenePackPresentProgressbar,
    DailyPrayerBoostPresentProgressbar,
    CryptonToCharityProgressBar,
    SpecialDealsAchievementProgressbar"
 )
