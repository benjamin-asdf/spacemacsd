
(use-package
    csharp-electric
  :demand t
  :config
  (progn
    (spacemacs/declare-prefix-for-mode
      'team/chsarp-superior-mode
      "op" "parameters")

    (spacemacs/declare-prefix-for-mode
      'team/chsarp-superior-mode
      "ox" "text")

    (spacemacs/declare-prefix-for-mode
      'team/chsarp-superior-mode
      "on" "new")

    (spacemacs/declare-prefix-for-mode
      'team/chsarp-superior-mode
      "oc" "classes")

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
      "ocr" #'benj/narrow-idlegame-const-buff)

    (spacemacs/set-leader-keys-for-minor-mode
      'team/chsarp-superior-mode
      "oxi" #'team-csharp/wrap-if
      "oxl" #'team-csharp/turn-conditional-to-log
      "oxr" #'my/chsarp-raise-block
      "onc" #'benj-create-new-csharp-scratch
      "onC" #'benj-csharp-scratches-create-on-region
      "oU" benj-csharp/replace-wl-with-debug
      "ou" benj-csharp/replace-wl-with-debug

      ))

  )



(provide 'csharp-config)



