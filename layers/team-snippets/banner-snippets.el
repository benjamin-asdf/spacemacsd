
(defun team-congrats/add-banner ()
  "Add snippets for a new banner."
  (interactive)
  (save-some-buffers)
  (let ((expand-env
         `((name ,(read-string "Congrats banner name: "))
           (cnt ,(team-congrats/source-count)))))
    (team-yassnippets/add-snippet-at-place
    team-congrats/view-file
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
     expand-env)))


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
