(require 'ert)

(defvar example-set-sprite-output)

(ert-deftest sprite-loader-simple-syntax ()
  (assert
   (string-equal
    "injectedC.LoadSpriteAsync(LoaderName.HolyBookSpritesLoader,\"AthenePack_\" + type,m.athenePackView.athenePackBackground);"
    (with-temp-buffer
      (insert
       "injectedC.SetSprite(MenuType.Church, SpriteContainer.HolyBookSprites, \"AthenePack_\" + type, m.athenePackView.athenePackBackground);")
      (->gg)
      (replace-sprite-loader-syntax)
      (buffer-string)))))

(ert-deftest sprite-syntax-with-field ()
  (should
   (string-equal
    (with-temp-buffer
      (insert "bestSprites.SetSprite(spriteName, trophy);")
      (->gg)
      (let ((cos-investigated-file t)
            (cos-override-resolve-sprite-loader-field-return-value "PvPSprites"))
        (replace-sprite-loader-syntax)
        (buffer-string)))
    "c.LoadSpriteAsync(LoaderName.PvPSpritesLoader,spriteName,trophy);")))

(defun replace-sprite-loader-syntax-cmd ()
  (interactive)
  (let ((cos-investigated-file
         "/home/benj/idlegame/IdleGame/Assets/#/Sources/Garrison/Monobehaviours/GarrisonLBEntryView.cs"))
    (replace-sprite-loader-syntax)))

(defvar cos-override-resolve-sprite-loader-field-return-value nil)
