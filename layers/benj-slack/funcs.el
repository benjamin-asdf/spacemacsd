

;; TODO figure out how to eval after slack
(auth-source-pass-enable)

(slack-register-team  :name "singularity-group"
                      :default t
                      :client-id "benjamin.schwerdtner@gmail.com"
                      :client-secret (auth-source-pick-first-password
                                      :host "slack.com"
                                      :user "benjamin.schwerdtner@gamil.com")
                      :token (auth-source-pick-first-password :host "singularity-group-slack"
                                                              :user "benjamin.schwerdtner@gamil.com")
                      :subscribed-channels '(general slackbot))

(setq slack-prefer-current-team t)
(setq slack-thread-also-send-to-room nil)

(slack-start)

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


;; all of this already exists
;;`slack-message-custom-notifier'

(defun benj/slack-notifier (message room team)
  "Custom notifier using notifiy send and a sound."
  ;; TODO I want to use alert.el
  ;; see `slack-message-notify-alert'
  (if (slack-message-notify-p message room team)
      ;; hack
      (let* ((team-name (or (oref team name) "SingularityGroup"))
            (room-name (slack-room-name room team))
            (text (with-temp-buffer
                    (goto-char (point-min))
                    (insert (slack-message-to-alert message team))
                    (slack-buffer-buttonize-link)
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))
            (user-name (slack-message-sender-name message team))
            (title (if (slack-im-p room)
                       (funcall slack-message-im-notification-title-format-function
                                team-name room-name (slack-thread-message-p message))
                     (funcall slack-message-notification-title-format-function
                              team-name room-name (slack-thread-message-p message))))
            (body (if (slack-im-p room) text (format "%s: %s" user-name text))))
        (if (and (eq alert-default-style 'notifier)
                 (slack-im-p room)
                 (or (eq (aref text 0) ?\[)
                     (eq (aref text 0) ?\{)
                     (eq (aref text 0) ?\<)
                     (eq (aref text 0) ?\()))
            (setq text (concat "\\" text)))

        ;; the only reason this exists is because I didnt' check `alert' enough to understand how to make it throw more
        ;; in you face notifications
        (notifications-notify :body body
               :icon slack-alert-icon
               :title title
               :sound-file (rand-element (split-string (shell-command-to-string (format "fd -I -e wav . %s" idlegame-project-root))))
               :urgency 'critical)

        (alert body :icon slack-alert-icon :title title :category 'slack)))

  ;; (alert (format "New slack message: %s" message) :title room)
  ;; (notifications-notify :title room :body message :icon slack-alert-icon :title )
  ;; (start-process "benj-slack-notifiy-sound" "*slack-notify-sound*" "aplay" (rand-element (split-string (shell-command-to-string (format "fd -I -e wav . %s" idlegame-project-root)))))
  )

(setq slack-message-custom-notifier 'benj/slack-notifier)

(defun benj-slack/upload-snippet-on-region ()
  "Create a temp file on region, call `slack-file-upload' with it."
  (interactive)
  (let ((file (team-create-temp-file-on-region)))
    ;; todo if the behaviour is too shit, then I take their room select thing there and open the buffer
    (slack-im-select)
    (benj-slack-updload file)))
