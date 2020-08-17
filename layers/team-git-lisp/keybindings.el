


(team/spacemacs-define-keys
 "og"
 "git"
 '("s" . vc-revision-other-window)
 '("c" . vc-find-conflicted-file)
 '("n" . benj-git/goto-next-unmerged-cs-file)

 ;; not really needed because we have the snippet
 ;; '("r" . (lambda () (interactive) (team-curr-revision-as-kill nil nil)))
 ;; '("R" . (lambda () (interactive) (team-curr-revision-as-kill nil t)))
 ;; '("b" . (lambda () (interactive) (team-curr-revision-as-kill t nil)))
 ;; '("B" . (lambda () (interactive) (team-curr-revision-as-kill t t)))


 '("D" . benj-git/diff-files-only)
 '("f" . magit-file-checkout)
 '("C" . benj-quick-commit)
 '("a" . team/magit-common-ancestor)
 '("F" . benj-git/fetch-and-merge)
 '("'" . (lambda (&optional arg) (interactive"P") (funcall (if arg #'benj-git/reset-modules #'benj-git/update-modules))))
 '("e" . benj-magit/ediff-resolve)
 '("y" . benj-git/yank-first-unmerged-file)
 '("0" . comm)
 '("r" . team/magit-unstage-regex)
 '("o" . team/magit-fetch-any)
 '("R" . team/magit-rebase-onto)

 )


(spacemacs/declare-prefix "ogi" "git-idlegame")
(spacemacs/set-leader-keys "ogic" 'benj-make-merge-request-commit)



(spacemacs/declare-prefix "ogh" "hunk")

(--each
    '(("r" git-gutter+-revert-hunks)
      ("s" git-gutter+-show-hunk)
      ("S" git-gutter+-show-hunk-inline-at-point))

 (spacemacs/set-leader-keys (concat "ogh" (car it)) (cadr it)))
