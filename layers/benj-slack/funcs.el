

;; TODO figure out how to eval after slack
(auth-source-pass-enable)


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



(defun benj-slack-updload (file &optional file-type)
  "Upload FILE, if FILE-TYPE is not given, read from user."
  (slack-file-upload file
                     (or file-type (slack-file-select-filetype (file-name-extension file)))
                     (read-from-minibuffer "Filename: " (file-name-nondirectory file))))


(defun benj-latest-screenshot ()
  "Get latest file located at \"~/Pictures/\" "
  (latest-file "~/Pictures"))


(defun benj-slack-upload-latest-screenshot ()
  "Upload the latest file in pictures dir."
  (interactive)
  (benj-slack-updload (benj-latest-screenshot) "png"))
