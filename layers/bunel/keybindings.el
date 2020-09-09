

(team/spacemacs-declare-keys
    "ou"
    "unity"
  "c" #'bunel/refresh-and-play
  "a" #'bunel-refresh-all
  "L" #'bunel-open-unity-editor-log
  "e" #'bunel-execute-debug-method
  "o" #'bunel-open-overlay
  "m" #'bunel-open-menu
  "p" #'bunel-open-prefab
  "d" #'bunel-open-debug-panel
  "g" #'bunel-set-globals
  "u" #'benj-unity/quick-file-usages
  "U" #'benj-unity/file-usages-with-guid-at-point
  "l" #'bunel/set-default-project
  "L" #'bunel/loading-scene-and-play
  "0" #'bunel-open-unity-editor-log
    )

(spacemacs/declare-prefix
  "out"
  "tests")
(spacemacs/set-leader-keys
  "outb" 'bunel/unity-unit-test-buffer
  "outl" 'bunel/unity-unit-test-last
  ;; todo at point
  "outt" 'bunel/unity-test-at-point
  )
