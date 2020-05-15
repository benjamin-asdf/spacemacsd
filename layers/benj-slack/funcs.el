(with-eval-after-load 'slack
  (slack-register-team :name "singularity-group"
                       :default t
                       :client-id "benjamin.schwerdtner@gmail.com"
                       :client-secret (auth-source-pick-first-password
                                       :host "slack.com"
                                       :user "benjamin.schwerdtner@gamil.com")
                       :token (auth-source-pick-first-password :host "singularity-group-slack"
                                                               :user "benjamin.schwerdtner@gamil.com")
                       :subscribed-channels '(general slackbot))

  (setq slack-prefer-current-team t))
