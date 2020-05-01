






















(defconst benj-roslyn-leader-keys "oy")

(spacemacs/declare-prefix benj-roslyn-leader-keys "roslyn")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-roslyn-leader-keys (car x)) (cdr x)))
        '(("b" . (lambda () (interactive) (benj-roslyn--build-proj-worker "Debug")))
          ("B" . (lambda () (interactive) (benj-roslyn--build-proj-worker "Release")))
          ("i" . benj-roslyn-run-default-idlegame)
          ("p" . benj-roslyn-run-playground)
          ("r" . sharpel-logsyntax-req)
          ("s" . sharpel-refresh-proc)
          ("f" . sharpel-send-file-name-command)
          ("l" . sharpel-rerun-last)
          ("F" . sharpel-rerun-last-file-command)
          )))
