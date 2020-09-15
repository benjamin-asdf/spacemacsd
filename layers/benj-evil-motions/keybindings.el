(team/spacemacs-declare-keys
    "om"
    "motions"
      "f" #'benj-avy/take-region
      "i" #'benj-insert-other-line
      "w" #'benj-avy/take-word
      "c" #'benj-avy/copy-word
      "r" #'avy-move-region
      "l" #'avy-move-line
      "R" #'benj-avy/move-region
      "n" #'my/insert-evil-mc-nums-simple


    )


(team/spacemacs-declare-keys
    "ot"
    "devel & misc"
  ;; t is reserved - temp kbd
  "d" #'my/assign-temp-kbd

    )


(team/spacemacs-declare-keys
    "oe"
    "transformations"
  "r" 'spacemacs/redshiftel-transient-state/body

    )

(team/spacemacs-declare-keys
    "oes"
    "substitute"
  "l" #'my/re-left-right
  )
