(team/spacemacs-declare-keys
    "om"
    "motions"
      "f" #'benj-avy/take-region
      "i" #'benj-insert-other-line
      "w" #'benj-avy/take-word
      "c" #'benj-avy/copy-word
      "r" #'avy-move-region
      "l" #'avy-move-line
      "R" #'benj-avy/move-region
      "n" #'my/insert-evil-mc-nums-simple
      "v" #'my/evil-visual-line-around-here
      "I" #'my/evil-mc-make-cursors-around-here
    )

(defun test ()
  (interactive)
  (message "Override the function definition of test first"))

(team/spacemacs-declare-keys
    "ot"
    "devel & misc"
  ;; t is reserved - temp kbd
  "d" #'my/eval-and-bind-func
  "m" #'my/make-cmd-wrapper
  ;;
  "e" #'test
  "x" #'my/make-template-string-from-region
  "o" #'my/sexp-to-interactive-func
  )

(team/spacemacs-declare-keys
    "os"
    "search & symbols"
  "o" #'my/jump-to-last-symbol-overlay
  )

(team/spacemacs-declare-keys
    "oe"
    "transformation, external"
  "r" #'spacemacs/redshiftel-transient-state/body
  "k" #'my/clear-konsoles
    )

(team/spacemacs-declare-keys
    "oy"
    "copy etc"
  "w" #'my/copy-symbol-other-window)

(team/spacemacs-declare-keys
    "oes"
    "substitute"
  "l" #'my/re-toggle-left-right
  "h" #'my/re-toggle-pet-hero
  "g" #'my/re-toggle-green-red
  "m" #'my/re-toggle-menu-overlay
  "," #'my/re-commata-newline
  "w" #'my/re-toggle-week-day
  "t" #'my/re-toggle-true-false
  "u" #'my/re-toggle-up-down
  "b" #'my/re-toggle-target-actionButton
  "o" #'my/re-toggle-online-offline
  "v" #'my/re-toggle-window-overlay
  )

(team/spacemacs-declare-keys
    "oep"
    "paramaters"
  "e" #'team/csharp-eldoc-to-param
  "y" #'team/insert-yank-as-param

  )

(spacemacs/set-leader-keys
  "nj"
  #'my/narrow-to-evil-pair)

(team/spacemacs-declare-keys
    "of"
    "files"
  "C" #'my/jump-last-bookmark
  "c" #'my/jump-last-bookmark-this-slot
  )

(spacemacs/set-leader-keys "cs" #'my/comment-or-uncomment-sexpr)
