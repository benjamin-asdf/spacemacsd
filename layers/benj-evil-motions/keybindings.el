


(defconst benj-motions-leader-keys "om")

(spacemacs/declare-prefix benj-motions-leader-keys "motions")

;; TOOD later today when I'm not sharp anymore

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-motions-leader-keys (car x)) (cdr x)))
        '(("f" . benj-avy/take-region)
          ("i" . benj-insert-other-line)
          ("n" . benj-next-digit)
          ("w" . benj-avy/take-word)
          ("e" . benj-avy/take-word-part)
          ("r" . avy-move-region)
          ("l" . avy-move-line)
          ("l" . avy-move-line)


          )))
