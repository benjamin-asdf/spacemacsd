


(defconst bunel-leader-keys "ou")

(spacemacs/declare-prefix bunel-leader-keys "unity")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat bunel-leader-keys (car x)) (cdr x)))
        '(("s" . bunel-save-and-refresh)
          ("S" . (lambda () (interactive) (bunel-save-and-refresh "with-playmode")))
          ("c" . bunel-refresh-client)
          ("C" . (lambda () (interactive) (bunel-refresh-client "with-playmode")))
          ("a" . bunel-refresh-all)
          ("A" . (lambda () (interactive) (bunel-refresh-all "with-playmode")))
          ("L" . bunel-open-unity-editor-log)
          ("e" . bunel-execute-debug-method)
          ("o" . bunel-open-overlay)
          ("m" . bunel-open-menu)
          ("p" . bunel-open-prefab)
          ("d" . bunel-open-debug-panel)
          ("g" . bunel-set-globals)
          ("u" . benj-unity/quick-file-usages)
          ("U" . benj-unity/file-usages-with-guid-at-point)
          ("l" . bunel/set-default-project)

          )))



(spacemacs/declare-prefix
  "out"
  "tests")
(spacemacs/set-leader-keys
  "outb" 'bunel/unity-unit-test-buffer
  "outl" 'bunel/unity-unit-test-last
  ;; todo at point
  "outt" 'bunel/unity-test-at-point
  )
