(defun team/git-update-modules (&optional arg)
  (interactive"P")
  (funcall (if arg #'benj-git/reset-modules #'benj-git/update-modules)))

(defun team/magit-stash-curr ()
  (interactive)
  (magit-stash-save "curr" t t nil t))

(team/spacemacs-declare-keys "og" "git"
 "s" #'vc-revision-other-window
 "n" #'benj-git/goto-next-unmerged-cs-file
 "D" #'benj-git/diff-files-only
 "C" #'benj-quick-commit
 "a" #'team/magit-common-ancestor
 "F" #'benj-git/fetch-and-merge
 "'" #'team/git-update-modules
 "e" #'benj-magit/ediff-resolve
 "o" #'team/magit-fetch-any
 "R" #'team/magit-rebase-onto
 "z" #'team/magit-stash-curr)

(team/spacemacs-declare-keys
    "ogf"
    "files"
  "f" #'magit-file-checkout
  "u" #'team/magit-unstage-files
  "d" #'team/magit-clean-files
  "a" #'team/magit-add-untracked
  "c" #'team/magit-checkout-changed
  "s" #'team/magit-add-files
  )

(team/spacemacs-declare-keys
    "ogl"
    "log"
  "l" #'team/magit-log-rev
  "r" #'team/magit-log-double-dot
  "s" #'benj/magit-show-commit-no-limit-files
  )

(team/spacemacs-declare-keys
    "ogc"
    "checkout etc"
  "f" #'team-magit/checkout-annoying
  )

(team/spacemacs-declare-keys "ogi" "git-idlegame"
  "c" #'benj-make-merge-request-commit)

(team/spacemacs-declare-keys "ogh" "hunk"
  "r" #'git-gutter+-revert-hunks
  "s" #'git-gutter+-show-hunk
  "S" #'git-gutter+-show-hunk-inline-at-point)

(team/spacemacs-declare-keys "ogu" "merging"
  "f" #'team/list-current-unmerged-files
  "i" #'benj-git/resolve-conflicts-interactive
  "p" #'benj-merge-prefabs
  "l" #'team/magit-log-merge
  "T" #'team/magit-all-theirs
  "O" #'team/magit-all-ours
  "s" #'team/list-current-unmerged-status
  "c" #'vc-find-conflicted-file
  "y" #'benj-git/yank-first-unmerged-file
  )
