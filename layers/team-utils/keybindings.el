(team/spacemacs-define-keys
 "o0"
 "utils"
 '("r" . team/regex-builder-with-region)
 ;; '("m" . spacemacs/macrostep-transient-state/macrostep-expand)
 )

(team/spacemacs-define-keys
 "or"
 "registers etc."
 '("a" . team/toggle-yank-to-letter)

 )

(spacemacs/set-leader-keys
  "km"
  'spacemacs/macrostep-transient-state/macrostep-expand)
