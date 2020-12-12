
(require 'csharp-transformations)

(spacemacs/declare-prefix-for-mode
  'team/chsarp-superior-mode
  "op" "parameters")

(spacemacs/declare-prefix-for-mode
  'team/chsarp-superior-mode
  "ox" "text")

(spacemacs/set-leader-keys-for-minor-mode
  'team/chsarp-superior-mode
  "C" #'team/catch-comp-on-line
  "c" #'team-electric/do-comp-helm
  "," #'team-electric/copy-word-to-reg
  "h" #'team-electric/helm-insert-comp-name
  "m" #'team-electric/yank-comp-name
  "opp" #'my-lazy/team/csharp-eldoc-to-param
  "opy" #'team/insert-yank-as-param
  "ot" #'cos/regenerate-gtags-background
  "opl" #'my-lazy/team/csharp-eldoc-expand-args
  "od" #'ggtags-find-definition
  )

(spacemacs/set-leader-keys-for-minor-mode
  'team/chsarp-superior-mode
  "oxi" #'team-csharp/wrap-if
  "oxl" #'team-csharp/turn-conditional-to-log
  )



;; (team/spacemacs-declare-keys
;;     "oep"
;;     "paramaters"

;;   )
