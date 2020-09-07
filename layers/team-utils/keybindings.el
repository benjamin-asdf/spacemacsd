(team/spacemacs-define-keys
 "o0"
 "utils"
 '()
 ;; '("m" . spacemacs/macrostep-transient-state/macrostep-expand)
 )

(team/spacemacs-define-keys
 "or"
 "registers etc."
 '()

 )

(team/spacemacs-declare-keys
    "o0"
    "utils"
    "r" #'team/regex-builder-with-region
    )

(team/spacemacs-declare-keys
    "or"
    "registers etc"
  "r" #'benj-copy-last-yank-to-register
  "b" #'team/pull-register-2-to-b
  "e" #'team/last-eldoc-csharp-no-type
  "a" #'team/toggle-yank-to-letter)

(spacemacs/set-leader-keys
  "km"
  'spacemacs/macrostep-transient-state/macrostep-expand)
