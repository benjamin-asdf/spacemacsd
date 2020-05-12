






















(defconst benj-roslyn-leader-keys "ok")

(spacemacs/declare-prefix benj-roslyn-leader-keys "roslyn")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-roslyn-leader-keys (car x)) (cdr x)))
        '(("b" . (lambda () (interactive) (benj-roslyn--build-proj-worker "Debug")))
          ("B" . (lambda () (interactive) (benj-roslyn--build-proj-worker "Release")))
          ("i" . benj-roslyn-run-idlegame)
          ("p" . benj-roslyn-run-playground)
          ("r" . sharpel-logsyntax-req)
          ("s" . sharpel-refresh-proc)
          ("F" . sharpel-send-file-name-command)
          ("l" . sharpel-rerun-last)
          ("f" . sharpel-rerun-last-file-command)
          ("d" . sharpel-file-command-on-region)
          ("c" . sharpel-rewrite-file)
          )))
