

(defconst benj-slack-leader-keys "o;")

(spacemacs/declare-prefix benj-slack-leader-keys "slack")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-slack-leader-keys (car x)) (cdr x)))
        '(("u" . slack-file-upload)
          ("s" . benj-slack-upload-latest-screenshot)
          ("c" . benj-slack/upload-snippet-on-region)
          ("b" . slack-message-write-another-buffer))))
