(require 'csharp-parsing)

(ert-deftest parse-arg-list-simple ()
  (should
   (equal
    (team-csharp-parse-arg-list
     "foo,bar")
    '("foo" "bar"))))

(ert-deftest parse-arg-list-nested ()
  (should
   (equal
    (team-csharp-parse-arg-list
     "foo,bar(bam,boo),lul")
    '("foo" "bar(bam,boo)" "lul"))))

(ert-deftest parse-arg-list-complex ()
  (should
   (equal
    (team-csharp-parse-arg-list
     "MenuType.Garrison, SpriteContainer.WeeklyEventProgressSliderSprites, SpriteName.BAR_FILLS[(int)rarity], slicedFill")
    '("MenuType.Garrison" "SpriteContainer.WeeklyEventProgressSliderSprites" "SpriteName.BAR_FILLS[(int)rarity]" "slicedFill"))))

;; ;; (team-csharp-parse-arg-list "MenuType.Garrison, SpriteContainer.WeeklyEventProgressSliderSprites, SpriteName.BAR_FILLS[(int)rarity]")
