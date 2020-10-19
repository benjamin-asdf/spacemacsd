

(defconst benj-slack-leader-keys "o;")

(spacemacs/declare-prefix benj-slack-leader-keys "slack")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-slack-leader-keys (car x)) (cdr x)))
        '(("u" . slack-file-upload)
          ("s" . benj-slack-upload-latest-screenshot)
          ("v" . benj-slack-upload-latest-vid)
          ("c" . benj-slack/upload-snippet-on-region)
          ("b" . slack-message-write-another-buffer))))


(dolist (mode '(slack-mode slack-message-buffer-mode slack-thread-message-buffer-mode))
  (spacemacs/set-leader-keys-for-major-mode mode
    "j" 'slack-channel-select
    "g" 'slack-group-select
    "r" 'slack-select-rooms
    "d" 'slack-im-select
    "p" 'slack-room-load-prev-messages
    "e" 'slack-message-edit
    "t" 'slack-thread-show-or-create
    "q" 'slack-ws-close
    "mm" 'slack-message-embed-mention
    "mc" 'slack-message-embed-channel
    "k" 'slack-select-rooms
    "@" 'slack-message-embed-mention
    "#" 'slack-message-embed-channel
    ")" 'slack-message-add-reaction
    "(" 'slack-message-remove-reaction)
  (let ((keymap (symbol-value (intern (concat (symbol-name mode) "-map")))))
    (evil-define-key 'insert keymap
      (kbd "@") 'slack-message-embed-mention
      (kbd "#") 'slack-message-embed-channel

      ;; override emojis, takes long and there is a bug atm
      (kbd ":") 'self-insert-command)))
