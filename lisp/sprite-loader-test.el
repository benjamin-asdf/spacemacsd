(ert-deftest id-when ()
  (assert
   (and
    (id-when 10 #'evenp)
    (id-when 11 #'oddp)
    (not (id-when 11 #'evenp)))))




(defvar
  example-set-sprite
  "injectedC.SetSprite(MenuType.Church, SpriteContainer.HolyBookSprites, \"AthenePack_\" + type, m.athenePackView.athenePackBackground);")

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



(defun here-test ()
  (interactive)
  (dump--replace-sprite-loader-syntax)
  )


(team-unity/field-ref-search
 "pvpSprites"
 "/home/benj/idlegame/IdleGame/Assets/#/Sources/Leaderboards/Separate/Monobehaviours/LBArenaScoreView.cs"
 )



(cos/cs-fiels-with-matches
 "SetSprite("
 )


(setq expected-out
      "c.LoadSpriteAsync(LoaderName.AbilityIconsSpritesLoader,spriteName,trophy);")

(ert-deftest sprite-syntax-with-field ()

    (flet ((resolve-sprite-loader-field
            (arg &optional arg1)
            "PvPSprites"))
      (with-temp-buffer
        (insert "sprites.SetSprite(spriteName, trophy);")
        (->gg)
        (dump--replace-sprite-container-invocation)

        (assert
         (string-equal
          "c.LoadSpriteAsync(LoaderName.PvPSpritesLoader,spriteName,trophy);"
          (buffer-string))))


      ))
