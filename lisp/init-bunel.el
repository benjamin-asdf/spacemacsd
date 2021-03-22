
(team/spacemacs-declare-keys
    "ou"
    "unity"
  "c" #'bunel-refresh-and-play
  "a" #'bunel-refresh-all
  "L" #'bunel-open-unity-editor-log
  "e" #'bunel-execute-debug-method
  "o" #'bunel-open-overlay
  "m" #'bunel-open-menu
  "g" #'bunel-set-globals
  "u" #'team-unity/rg-guid-search
  "U" #'team-unity/rg-guid-search-at-point
  "f" #'team-unity/rg-guid-search-ask-file
  "0" #'bunel-set-default-project
  "L" #'bunel-loading-scene-and-play
  "0" #'bunel-open-unity-editor-log
  "O" #'team-unity/open-prefab-at-point
    )

(team/spacemacs-declare-keys
    "oud"
    "debug"
  "p" #'bunel-open-debug-panel
  "l" #'bunel-set-debug-lore

  )

(team/spacemacs-declare-keys
  "oup"
  "prefabs"
  "o" #'bunel-open-prefab
  "s" #'bunel-scene-view
  "c" #'cos/do-prefab-integrity-check
  "C" #'cos/check-conflicted
  "m" #'cos/prefab-integrity-check
  "l" #'team-unity/lazy-do-add-label

  )



(team/spacemacs-declare-keys
  "out"
    "tests"
  "b" #'bunel-unity-unit-test-buffer
  "l" #'bunel-unity-unit-test-last
  ;; todo at point
  "t" #'bunel-unity-test-at-point
  )


(team/spacemacs-declare-keys
    "oul"
    "last"
  "l" #'bunel-rerun-last
  "e" #'bunel-rerun-debug-method)
