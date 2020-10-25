("\"AthenePack_\" + type, m.athenePackView.athenePackBackground);")

(defvar example-set-sprite-output
  "injectedC.LoadSpriteAsync(LoaderName.HolyBookSpritesLoader, \"AthenePack_\" + type, m.athenePackView.athenePackBackground);")


(ert-deftest sprite-loader-simple-syntax ()
  (assert
   (string-equal
    example-set-sprite-output
    (with-temp-buffer
      (insert example-set-sprite)
      (->gg)
      (dump--replace-sprite-loader-syntax)
      (buffer-string)))))



(ert-deftest sprite-container-invocation-syntax ()

  (flet ((resolve-sprite-loader-field (arg &optional arg1)
                                      ("PvPSprites")

                                      ))

    )

  )


(defun do-test ()
  (interactive)
  (with-current-buffer-window
      (get-buffer-create "out")
      nil
      nil
    (erase-buffer)
    (insert example-set-sprite)
    (->gg)
    (dump--replace-sprite-loader-syntax)
    (buffer-string)))


;;;  try replace the frist 100



;;;  see the issues

;;;  code

;;;  replace until the first 100 compile

;;;  try 500 etc



(team-unity/field-ref-search
 "pvpSprites"
 "/home/benj/idlegame/IdleGame/Assets/#/Sources/Leaderboards/Separate/Monobehaviours/LBArenaScoreView.cs")

(defvar
  "v.divisionBarSprites.SetSprite(LBSpriteNames.GetSliderTipName(barColor, true), unslicedTip);")

(team/with-default-dir
 idlegame-assets-dir
 (team/each-file
  (-difference
   (-take
    100
    (cos/cs-fiels-with-matches
     "SetSprite\\(.*,"))
   spriteloaders-skip-files)
  (or
   (dump--replace-sprite-loader-syntax)
   (progn
     (->gg)
     (dump--replace-sprite-container-invocation)))))



(defvar cos-override-resolve-sprite-loader-field-return-value nil)

(ert-deftest sprite-syntax-with-field ()
  (should
   (string-equal
    (with-temp-buffer
      (insert "sprites.SetSprite(spriteName, trophy);")
      (->gg)
      (let ((inhibit-read-only t)
            (cos-investigated-file "")
            (cos-override-resolve-sprite-loader-field-return-value "PvPSprites"))
        (dump--replace-sprite-container-invocation))
      (buffer-string))
    "c.LoadSpriteAsync(LoaderName.PvPSpritesLoader, trophy);")))
