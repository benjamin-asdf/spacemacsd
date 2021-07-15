(team/spacemacs-declare-keys
    "ok" "roslyn"
  "b" #'benj-roslyn-tools/nuke-build
  "k" #'benj-roslyn-build-analyzers
  "w" #'benj-roslyn-tools/pop-to-nuke-buff
  "s" #'benj-roslyn-tools/pop-to-analyzer-log
  "S" #'benj-roslyn-tools/erase-analyzer-log-buff-if-exists

  "P" #'benj-roslyn-tools/run-playground-startup
  "u" #'benj-roslyn-tools/run-idlegame-sync

  "v" #'benj-roslyn-tools/build-banned-analzyer

  "i" #'benj-roslyn-tools/run-idlegame-default
  "I" #'benj-roslyn-tools/run-idlegame
  "y" #'benj-roslyn/run-idlegame-sync

  "p" #'benj-roslyn-run-playground
  "A" #'benj-roslyn-tools/do-run-analyzers
  "g" #'benj-roslyn-tools/one-shot-playground
  "a" #'benj-roslyn-rerun-last
  "l" #'benj-roslyn-tools/log-goto-warning-location

  "t" #'benj-roslyn-tools/goto-test-create-if-not-exists

  "h" #'benj-rosly-tools/one-shot-idlegame
  "c" #'benj-csharp-get-diagnostic-args

  )
