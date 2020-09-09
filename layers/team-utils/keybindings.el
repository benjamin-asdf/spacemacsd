
(defalias 'team/pop-to-stack-buff
  (// () (interactive) (pop-to-buffer team/stack-buff)))

(team/spacemacs-define-keys
 "o0"
 "utils"
 '()
 ;; '("m" . spacemacs/macrostep-transient-state/macrostep-expand)
 )

(team/spacemacs-declare-keys
    "o0"
    "utils"
    "r" #'team/regex-builder-with-region
    "m" #'macrostep-expand
    )

(team/spacemacs-declare-keys
    "or"
    "registers etc"
  "r" #'benj-copy-last-yank-to-register
  "b" #'team/pull-register-2-to-b
  "e" #'team/last-eldoc-csharp-no-type
  "a" #'team/toggle-yank-to-letter
  "o" #'team/push-region-to-stack
  "p" #'team/pop-stack-buff
  )

(team/spacemacs-declare-keys
    "obs"
    "scratch-buffs"
  "s" #'team/pop-to-stack-buff
  )

(spacemacs/set-leader-keys
  "km"
  'spacemacs/macrostep-transient-state/macrostep-expand)
