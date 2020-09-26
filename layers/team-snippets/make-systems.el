(defun cos/make-dot-system (name)
  (unless
      (team/with-file
       "/home/benj/idlegame/IdleGame/Assets/#/Sources/GameGuide/GameGuideSystems.cs"
       (re-search-forward (format "%sGreenDotSystem" name) nil t))
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/GameGuide/GameGuideSystems.cs"
     "dot-source-system"
     "public class RemoveGreenDotsSystem"
     `((name ,name)))
    (team-yassnippets/add-snippet-at-place
     "/home/benj/idlegame/IdleGame/Assets/#/Sources/GameGuide/GameGuideSystems.cs"
     "feature-sys"
     "RemoveGreenDotsSystem"
     `((sys-name ,(format "%sDotSystem" name))
       (contexts "c")))))




(cos/make-dot-system "TableGameActive")
(cos/make-dot-system "TableGameReceivedLife")
(cos/make-dot-system "ArenaBulk")
(cos/make-dot-system "CampaignBulk")
(cos/make-dot-system "ChurchLive")
(cos/make-dot-system "ChurchPackActive")
(cos/make-dot-system "ChurchGiveawayActive")
(cos/make-dot-system "ChurchFreeSpin")
(cos/make-dot-system "ChurchPray")
(cos/make-dot-system "AttackStreamer")
(cos/make-dot-system "GarrisonMeta")
(cos/make-dot-system "GarrisonSpecial")
(cos/make-dot-system "GarrisonDailyVideo")
(cos/make-dot-system "CasinoFreeSpin")
(cos/make-dot-system "CasinoRefresh")
(cos/make-dot-system "AchievementsChest")
(cos/make-dot-system "BossTowerReplayReady")
(cos/make-dot-system "BossTowerCanBattle")
(cos/make-dot-system "BossTowerMissionCompleted")
(cos/make-dot-system "BlockchainTransaction")
(cos/make-dot-system "FaceswapNewFace")
(cos/make-dot-system "CommunityLive")
(cos/make-dot-system "CommunityClaim")
(cos/make-dot-system "MineExpeditionComplete")
(cos/make-dot-system "MineHeartRequest")
(cos/make-dot-system "MineCrystalCapacity")
(cos/make-dot-system "MineBagCapacity")
(cos/make-dot-system "GeopetDailyReset")
(cos/make-dot-system "GeopetCapReached")
(cos/make-dot-system "GoLiveStatus")
(cos/make-dot-system "StoriesDailyReset")
(cos/make-dot-system "StoriesMatchedYou")
(cos/make-dot-system "TalentTreeNewTalent")
(cos/make-dot-system "TalentTreeNewTree")
(cos/make-dot-system "Match3Rewards")
(cos/make-dot-system "Match3Egg")
(cos/make-dot-system "MarketplaceRefresh")
(cos/make-dot-system "MerchantDailyReset")
(cos/make-dot-system "MerchantDeal")
(cos/make-dot-system "SummonFreeCharge")
(cos/make-dot-system "SettingsPromotedSkins")
(cos/make-dot-system "SpecialEventsLogin")
(cos/make-dot-system "SpecialEventsLoginLastDay")
(cos/make-dot-system "SpecialEventsTrade")
(cos/make-dot-system "SpecialEventsVault")
(cos/make-dot-system "SpecialEventsPresents")
(cos/make-dot-system "SpecialEventsArtifact")
(cos/make-dot-system "BlackmarketMonthylReset")
(cos/make-dot-system "BlackmarketSecret")
(cos/make-dot-system "BlackmarketTrade")
(cos/make-dot-system "ForgePet")
(cos/make-dot-system "ForgeHero")
(cos/make-dot-system "FusionPet")
(cos/make-dot-system "FusionHero")
(cos/make-dot-system "FusionArtifact")
(cos/make-dot-system "CurrencyWidget")
(cos/make-dot-system "MailTreassureMailRedirect")
