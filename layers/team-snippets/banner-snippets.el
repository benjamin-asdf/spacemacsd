(defun team-congrats/add-banner ()
  "Add snippets for a new banner."
  (interactive)
  (save-some-buffers)
  (let* ((name (team/capitalize-first-letter
                (read-string "Congrats banner name: ")))
         (name-downcase (team/un-capitalize name))
         (expand-env
          `((name ,name)
            (cnt ,(team-congrats/source-count))
            (name-downcase ,name-downcase))))
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/MonoBehaviours/CongratulationsScreen.cs"
    "congrats-cb-dict"
    "CongratsSource.FirstPurchase, m => new FirstPurchaseCongratsCallbacks"
    expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/CongratsCallbacks.cs"
     "congrats-cb-class"
     "public class FirstPurchaseCongratsCallbacks "
     expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/CongratsSource.cs"
     "congrats-source-enum"
     "}"
     expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/CongratsComponents.cs"
     "animcomp"
     "public class FirstPurchaseCongrats : FlagComponent, IAnimFlag"
     expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/CongratsSource.cs"
     "congrats-source-anim-prio"
     " default:"
     expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/MoreBanners.cs"
     "congrats-system"
     "public static class CongratsBannersExt {"
     expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/MoreBanners.cs"
     "feature-sys-congrats"
     "Add(new SalePointCongratsSystem(c));"
     expand-env)
    (team/with-file
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/MonoBehaviours/CongratulationsScreen.cs"
     (when (re-search-forward "Banners, set by code" nil t)
       (team/in-new-line
        (format "public %sCongratsView %s;" name name-downcase)))
     (->gg)
     (when (re-search-forward
            "_renderMysteryReward(config)"
            nil
            t)
       (team/in-new-line
        (format "_render%s(config);" name))))
    (when (re-search-forward
           "salePointBanner.SetActive(false);"
           nil
           t)
      (skip-chars-forward "^{")
      (team/in-new-line
       (format "%s.SetActive(false);\n" name-downcase)))
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/MonoBehaviours/CongratulationsScreen.cs"
     "congrats-render"
     "void _renderCatchupLuck"
     expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/MoreBanners.cs"
     "congrats-ext-method"
     "public static void CatchupLuckCongrats"
     expand-env)
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/CheatTools/DebugMethods.cs"
     "congrats-debug-method"
     "public void MilestoneCongrats() {"
     expand-env)
    (eval `(let ,expand-env
       (let ((file-name (concat "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/MonoBehaviours/"
                                (format "%sCongratsView.cs" name))))
         (unless (file-exists-p file-name)
           (write-region

            (with-temp-buffer
              (csharp-mode)
              (team/chsarp-snippet-insert
               "congrats-viewmanager"
               ".*"
               expand-env)
              (buffer-string))
            nil
            file-name)))))
    (with-current-buffer
        "*scratch:org*"
      (insert "* Add references for %s" name)
      (org-mode)
      (pop-to-buffer (current-buffer))))
  (team/with-idlegame-git-toplevel
   (team/
    )
   (magit-run-git-async
    "add"
    "--"
    (list
     "IdleGame/Assets/#/Sources/CongratsScreen/MonoBehaviours/CongratulationsScreen.cs"
     "IdleGame/Assets/#/Sources/CongratsScreen/CongratsCallbacks.cs"
     "IdleGame/Assets/#/Sources/CongratsScreen/CongratsComponents.cs"
     "IdleGame/Assets/#/Sources/CongratsScreen/MoreBanners.cs"
     "IdleGame/Assets/#/Sources/CheatTools/DebugMethods.cs"))))


(defmacro team/with-idlegame-proj (&rest body)
  `(team/with-default-dir
    idlegame-project-root
    ,@body))

(defmacro team/with-idlegame-git-toplevel (&rest body)
  `(team/with-idlegame-proj
   (magit-with-toplevel
     ,@body)))

;; TOOD add to enum
(defun team-congrats/source-count ()
  (team/with-file
   "/home/benj/idlegame/IdleGame/Assets/#/Sources/CongratsScreen/CongratsSource.cs"
   (when (and (re-search-forward "}" nil t)
              (re-search-backward "\\w+ = \\([0-9]+\\)"))
     (string-to-number (match-string-no-properties 1)))))


(team/spacemacs-declare-keys
    "ot" "temp"
  "b" #'team-congrats/add-banner
  )
