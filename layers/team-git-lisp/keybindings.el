
(defconst team-git-leader-keys "og")

(spacemacs/declare-prefix team-git-leader-keys "git")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat team-git-leader-keys (car x)) (cdr x)))
        '(("s" . vc-revision-other-window)
          ("c" . vc-find-conflicted-file)
          ("n" . benj-git/goto-next-unmerged-cs-file)
          ("r" . (lambda () (interactive) (team-curr-revision-as-kill nil nil)))
          ("R" . (lambda () (interactive) (team-curr-revision-as-kill nil t)))
          ("b" . (lambda () (interactive) (team-curr-revision-as-kill t nil)))
          ("B" . (lambda () (interactive) (team-curr-revision-as-kill t t)))
          ("D" . benj-git/diff-files-only)
          ("f" . magit-file-checkout)
          ("C" . benj-quick-commit)
          ("a" . team-git-common-ancestor-as-kill)
          ("F" . benj-git/fetch-and-merge))))


(spacemacs/declare-prefix "ogi" "git-idlegame")
(spacemacs/set-leader-keys "ogic" 'benj-make-merge-request-commit)
