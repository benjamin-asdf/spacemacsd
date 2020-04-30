

(defun benj-roslyn--runner-worker (config args)
  "Run release project with ARGS.
CONFIG must be one of `benj-roslyn-proj-configs'
:release
:debug"
  (let ((buff-name "*roslyn-analzyers*")
        (process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/"))))
    (start-process
     "run-analyzers"
     buff-name
     "/usr/bin/mono"
     (benj-roslyn-cli-path config)
     args ;; TODO
     )
    (switch-to-buffer-other-window buff-name)))

;; works
;; TODO figure out, args need to be seperated
;; (let ((process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/"))))
;;   (start-process
;;    "run-analyzers"
;;    "*roslyn-analzyers*"
;;    "/usr/bin/mono"
;;    "/home/benj/idlegame/RoslynAnalyzers/EntityClosureCLI/bin/Release/EntityClosureCLI.exe"
;;    "-s" idlegame-sln-path
;;    "-x" "\"(Test)|(^Unity\\\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)\""
;;    "-i" "\".*\\Assets\\.*\""
;;    "--no-git"
;;    )
;;   )




;; (defun my-args-func (&rest args)
;;   ;; (mapconcat 'identity args )
;;   (start-process "procname" "procbuff" "fd" args)
;;   )
;; (my-args-func "." "/home/benj/")
;; (my-args-func benj-roslyn-idlegame-analyzer-args)
