
(spacemacs/declare-prefix-for-mode
  'team/chsarp-superior-mode
  "op" "parameters")

(spacemacs/set-leader-keys-for-minor-mode
  'team/chsarp-superior-mode
  "c" #'team/catch-comp-on-line
  "," #'team-electric/copy-word-to-reg
  "m" #'team-electric/yank-comp-name
  "opp" #'team/csharp-eldoc-to-param
  "opy" #'team/insert-yank-as-param)





;; (team/spacemacs-declare-keys
;;     "oep"
;;     "paramaters"

;;   )
