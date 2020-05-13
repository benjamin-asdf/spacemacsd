(defconst benj-roslyn-proj-path (concat (file-name-as-directory cos-dir) "RoslynAnalyzers"))
(defconst benj-cos-roslyn-sln-path (concat (file-name-as-directory benj-roslyn-proj-path) "RoslynAnalyzers.sln"))
(defconst benj-roslyn-cli-bin (concat (file-name-as-directory benj-roslyn-proj-path) "EntityClosureCLI/" "bin/"))
(defconst benj-roslyn-cli-name "EntityClosureCLI.exe")
(defconst idlegame-sln-path (concat idlegame-project-root "IdleGame.sln"))

;; TODO program doesn't parse that
(defconst benj-roslyn-idlegame-analyzer-args
  '("-x" "\"(Test)|(^Unity\\\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)\""
    "-i" "\".*\\Assets\\.*\""))


;; TODO
(defconst benj-roslyn-proj-configs
  '((:debug . "Debug")
    (:release . "Release")))

;; TODO
;; would be trivial to have some helm selection of args


(defun benj-roslyn--build-proj-worker (config)
  "Build cos roslyn project, CONFIG should be a string of the form 'Release' or 'Debug'."
  (benj-msbuild-sln benj-cos-roslyn-sln-path (concat config "Linux") (format "*build-roslyn-%s*" (concat config "Linux"))))


(defun benj-msbuild-sln (sln-path config &optional buff-name)
  "Build sln at SLN-PATH using mono msbuild. CONFIG is a string passed as /p:Configuration=
usually something like 'Release'.
Optional BUFF-NAME to put proc output in a custom buffer. "
  (let ((buff-name (or buff-name (format "*msbuild-%s*" config))))
    (start-process "benj-msbuild" buff-name "msbuild" sln-path (format "/p:Configuration=%s" config))
    (switch-to-buffer-other-window buff-name)))

(defun benj-roslyn-cli-path (config)
  "Roslyn cli path for CONFIG.
Meaningfull values for CONFIG are
:release
:debug
see `benj-roslyn-proj-configs'"
  (concat benj-roslyn-cli-bin
          (file-name-as-directory
           (cdr (assoc config benj-roslyn-proj-configs)))
          benj-roslyn-cli-name))


(defun benj-roslyn-run-playground ()
  "Run release build on playground project."
  (interactive)
  (benj-roslyn-runner
   benj-cos-roslyn-sln-path
   "-t" "Playground"))

(defun benj-roslyn-run-idlegame (&optional args)
  "Run release build on playground project. ARGS can be additional args."
  (interactive)
  (benj-roslyn-runner
   idlegame-sln-path
   ;; benj-roslyn-idlegame-analyzer-args ;TODO
   "-t" "Main"
   "-v"
   ;; "--no-git"
   ))


;; "-a" "StartupMethodAnalyzer" "-startup"
;; "-e" "UNITY_EDITOR"
;; "-p" "UNITY_IOS"


(defun benj-roslyn-runner (sln &rest args)
  "Run release analzyers with SLN and additional ARGS"
  (interactive)
  (let (
        (buff-name "*roslyn-analzyers*")
        (process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/"))))
    (benj-start-proccess-flatten-args
     "run-analyzers"
     buff-name
     "/usr/bin/mono"
     "/home/benj/idlegame/RoslynAnalyzers/EntityClosureCLI/bin/Release/EntityClosureCLI.exe"
     "-s" sln
     args)
    (pop-to-buffer buff-name)))

(defun benj-start-process-synchronously-flatten-args (name buffer program &rest program-args)
  "See `benj-start-proccess-flatten-args' use `accept-process-output' to run synchronously.
Returns the created proc."
  (let ((proc (benj-start-proccess-flatten-args name buffer program program-args)))
    (while (accept-process-output proc))
    proc))

(defun benj-start-proccess-flatten-args (name buffer program &rest program-args)
  "See `start-process'. Uses `make-process.'
Instead of consing PROGRAM and PROGRAM-ARGS, also flatten the list, see `-flatten'"
  (unless (fboundp 'make-process)
    (error "Emacs was compiled without subprocess support"))
  (apply #'make-process
	       (append (list :name name :buffer buffer)
		             (if program
		                 (list :command (-flatten (cons program program-args)))))))


