
(spacemacs/declare-prefix-for-mode
  'team/chsarp-superior-mode
  "op" "parameters")

(spacemacs/set-leader-keys-for-minor-mode
  'team/chsarp-superior-mode
  "C" #'team/catch-comp-on-line
  "c" #'team-electric/do-comp-helm
  "," #'team-electric/copy-word-to-reg
  "h" #'team-electric/helm-insert-comp-name
  "m" #'team-electric/yank-comp-name
  "opp" #'team/csharp-eldoc-to-param
  "opy" #'team/insert-yank-as-param)




;; (team/spacemacs-declare-keys
;;     "oep"
;;     "paramaters"

;;   )
