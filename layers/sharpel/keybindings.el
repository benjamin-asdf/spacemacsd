(defconst benj-roslyn-leader-keys "oz")

(spacemacs/declare-prefix benj-roslyn-leader-keys "sharpel")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-roslyn-leader-keys (car x)) (cdr x)))
        '(("r" . sharpel-logsyntax-req)
          ("s" . sharpel-refresh-proc)
          ("F" . sharpel-send-file-name-command)
          ("l" . sharpel-rerun-last)
          ("f" . sharpel-rerun-last-file-command)
          ("d" . sharpel-file-command-on-region)
          ("c" . sharpel-rewrite-file)
          ("t" . (lambda () (interactive) (sharpel--start "--try-compilation"))))))
