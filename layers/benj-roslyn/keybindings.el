






















(defconst benj-roslyn-leader-keys "ok")

(spacemacs/declare-prefix benj-roslyn-leader-keys "roslyn")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-roslyn-leader-keys (car x)) (cdr x)))
        '(
          ("b" . benj-roslyn-tools/nuke-build)
          ("c" . benj-roslyn-tools/nuke-clean)
          ("w" . benj-roslyn-tools/pop-to-nuke-buff)
          ("s" . benj-roslyn-tools/pop-to-analyzer-log)


          ;; TODO
          ("i" . benj-roslyn/run-idlegame-default)
          ("I" . benj-roslyn-run-idlegame)
          ("S" . benj-roslyn/run-idlegame-sync)
          ("p" . benj-roslyn-run-playground)
          ("A" . benj-roslyn-do-run)
          ("a" . benj-roslyn-rerun-last)

          ;; TODO adapt to nuke
          ;; build evrtim

          (";" . benj-roslyn//run-closure)
          )))
