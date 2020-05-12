(defconst benj-roslyn-proj-path (concat (file-name-as-directory cos-dir) "RoslynAnalyzers"))
(defconst benj-cos-roslyn-sln-path (concat (file-name-as-directory benj-roslyn-proj-path) "RoslynAnalyzers.sln"))
(defconst benj-roslyn-cli-bin (concat (file-name-as-directory benj-roslyn-proj-path) "EntityClosureCLI/" "bin/"))
(defconst benj-roslyn-cli-name "EntityClosureCLI.exe")
(defconst idlegame-sln-path (concat idlegame-project-root "IdleGame.sln"))

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
   benj-roslyn-idlegame-analyzer-args
   "--no-git"))

(benj-roslyn-run-idlegame)
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


(defconst sharpel-repo-root "/home/benj/repos/csharp/Sharpel/")
(defconst sharpel-proj-dir (concat sharpel-repo-root "Sharpel/"))
(defconst sharpel-sln-path (concat sharpel-repo-root "Sharpel.sln"))
(defconst sharpel-release-exe-dir (concat sharpel-proj-dir "bin/Release/netcoreapp3.0/"))
(defconst sharpel-debug-exe-dir (concat sharpel-proj-dir "bin/Debug/netcoreapp3.0/"))
(defconst sharpel-release-exe-path (concat sharpel-release-exe-dir "Sharpel.dll"))
(defconst sharpel-debug-exe-path (concat sharpel-debug-exe-dir "Sharpel.dll"))
(defconst sharpel-buff-name "*sharpel*")

(defvar sharpel-process nil)

(defun sharpel-start-proc ()
  "Start roslyn proc and switch to output buffer"
  (let ((default-directory sharpel-proj-dir))
    (setq sharpel-process (start-process "benj-roslyn" sharpel-buff-name "dotnet" "run"))
   (switch-to-buffer-other-window sharpel-buff-name)))

(defun sharpel-build (config)
  "Compile the sharpel project. CONFIG is either 'Release' or 'Debug' "
  (interactive)
  (sharpel-clean-proc)
  (benj-msbuild-sln sharpel-sln-path config))


(defun sharpel-clean-proc ()
  "Delete process and reset state."
  (interactive)
  (when (processp sharpel-process)
    (progn (delete-process sharpel-process) (setq benj-roslyn-process nil))))

(defun sharpel-ensure-proc ()
  "Ensure that there is a benj roslyn process"
  (unless (and sharpel-process (processp sharpel-process) (process-live-p sharpel-process))
          (sharpel-start-proc)))


(defun sharpel-refresh-proc ()
  "Refresh sharpel compilation. And create new proc."
  (interactive)
  ;; (sharpel-build "Debug") ;; dotnet run is good enough
  (sharpel-clean-proc)
  (sharpel-ensure-proc))


(defun sharpel-logsyntax-req ()
  "Send active region as logsyntax request"
  (interactive)
  (sharpel--runner
   (concat ":logsyntax:\n"
           (replace-regexp-in-string "[ \t\n\r]+" " " (buffer-substring (region-beginning) (region-end)))
           ;; (replace-regexp-in-string "[\n\r]+" (make-string 1 ?\0) (buffer-substring (region-beginning) (region-end)))
           "\n"))
  (org-mode))


(defun sharpel-file-command-on-region ()
  "Create a file command using a temp file created from region."
  (interactive)
  (sharpel--send-file-name-command (team-create-temp-file-on-region)))

(defconst sharpel-command-kinds
  '((:filename . ":filename:")
    (:logsyntax . ":logsyntax:")
    (:rewrite-file: ":rewrite-file"))
  "Possible cammands send to sharpel proc.")

(defvar sharpel-last-input nil)
(defvar sharpel-last-file-send nil)

;; TODO maybe select file, default to buffer file
(defun sharpel-send-file-name-command ()
  "Send current buffer file name command to sharpel."
  (interactive) ;; interactive list form
  (sharpel--send-file-name-command buffer-file-name))

(defun sharpel-rewrite-file (file-name)
  "Run sharpel with rewrite reqeust and FILE-NAME"
  (interactive "fFile for const rewrite: ")
  (sharpel-run-cmd :rewrite-file file-name))

(defun sharpel--send-file-name-command (file-name)
  "Send FILE-NAME as input to sharpel."
  (setq sharpel-last-file-send file-name)
  (sharpel-run-cmd :filename file-name))

(defun sharpel--command (cmd body)
  "Build sharpel command input. With header CMD and BODY.
Valid options for CMD are defined in `sharpel-command-kinds'."
  (concatenate 'string (cdr (assoc cmd sharpel-command-kinds)) "\n" body "\n"))


(defun sharpel-rerun-last-file-command ()
  "Rerun last file command, if set."
  (interactive)
  (if sharpel-last-file-send (sharpel--send-file-name-command sharpel-last-file-send)
    (message "No previous file send command.")))

(defun sharpel-run-cmd (cmd body)
  "See `sharpel--runner'. Run sharpel with command CMD, which must be one of `sharpel-command-kinds' and input BODY."
  (sharpel--runner (sharpel--command cmd body)))

(defun sharpel--runner (input)
  "Ensure sharpel and run input"
  (sharpel-ensure-proc)
  (setq sharpel-last-input input)
  (process-send-string sharpel-process input)
  (unless (string-equal (buffer-name) sharpel-buff-name)
    (pop-to-buffer sharpel-buff-name))
  (csharp-mode))

(defun sharpel-rerun-last ()
  "Rerun the last sharpel command."
  (interactive)
  (if sharpel-last-input (sharpel--runner sharpel-last-input)))
