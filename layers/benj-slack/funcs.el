




;; TODO
;; (require 'auth-source-pass)
(auth-source-pass-enable)
;; auth-source-pass didn't load somehow
;; or is auto sourc pass enable suffient, prob


(slack-register-team :name "singularity-group"
                      :default t
                      :client-id "benjamin.schwerdtner@gmail.com"
                      :client-secret (auth-source-pick-first-password
                                      :host "slack.com"
                                      :user "benjamin.schwerdtner@gamil.com")
                      :token (auth-source-pick-first-password :host "singularity-group-slack"
                                                              :user "benjamin.schwerdtner@gamil.com")
                      :subscribed-channels '(general slackbot))

(setq slack-prefer-current-team t)
