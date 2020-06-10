







(defconst benj-csharp-leader-keys "od")

(spacemacs/declare-prefix benj-csharp-leader-keys "chsarp-dotnet")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-csharp-leader-keys (car x)) (cdr x)))
        '(("p" . benj-chsarp-msbuild-this-proj)
          ("u" . benj-csharp/replace-wl-with-debug)
          ("c" . (lambda () (interactive) (benj//replace-match-buff "UnityEngine.Debug.Log" "Console\.WriteLine")))
          )))
