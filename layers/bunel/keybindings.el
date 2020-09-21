

(team/spacemacs-declare-keys
    "ou"
    "unity"
  "c" #'bunel/refresh-and-play
  "a" #'bunel-refresh-all
  "L" #'bunel-open-unity-editor-log
  "e" #'bunel-execute-debug-method
  "o" #'bunel-open-overlay
  "m" #'bunel-open-menu
  "d" #'bunel-open-debug-panel
  "g" #'bunel-set-globals
  "u" #'team-unity/rg-guid-search
  "U" #'team-unity/rg-guid-search-at-point
  "f" #'team-unity/rg-guid-search-ask-file
  "0" #'bunel/set-default-project
  "L" #'bunel/loading-scene-and-play
  "0" #'bunel-open-unity-editor-log
  "O" #'team-unity/open-prefab-at-point
    )

(team/spacemacs-declare-keys
  "oup"
  "prefabs"
  "o" #'bunel-open-prefab
  "s" #'bunel/scene-view
  )

(team/spacemacs-declare-keys
  "out"
    "tests"
  "outb" #'bunel/unity-unit-test-buffer
  "outl" #'bunel/unity-unit-test-last
  ;; todo at point
  "outt" #'bunel/unity-test-at-point
  )


(team/spacemacs-declare-keys
    "oul"
    "last"
  "l" #'bunel/rerun-last
  "e" #'bunel/rerun-debug-method

    )
