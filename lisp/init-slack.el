(require 'slack)

(load "~/.spacemacs.d/lisp/slack-config.el" )


(slack-register-team  :name "singularity-group"
                      :default t
                      :client-id "benjamin.schwerdtner@gmail.com"
                      ;; :client-secret (auth-source-pick-first-password
                      ;;                 :host "slack-passw"
                      ;;                 :user "benjamin.schwerdtner@gamil.com")
                      :token (auth-source-pick-first-password :host "slack-token"
                                                              :user "benjamin.schwerdtner@gmail.com")
                      :subscribed-channels '(general slackbot new-to-cos))


(run-at-time 500 nil #'slack-start)


(provide 'init-slack)
