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
