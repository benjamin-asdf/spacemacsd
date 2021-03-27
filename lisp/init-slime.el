(with-eval-after-load 'slime
  ;; makes it better together with cepl
  (setq slime-inhibit-pipelining nil)
)

(with-eval-after-load 'slime-repl
  (spacemacs/declare-prefix-for-mode
    'slime-repl-mode
    "l"
    "repl funcs")

  (spacemacs/set-leader-keys-for-major-mode
    'slime-repl-mode
    "ll"
    #'counsel-slime-repl-history)

  (team/spacemacs-declare-keys
      "ol"
      "lisp, other"
    "o" #'counsel-slime-repl-history))

(provide 'init-slime)
