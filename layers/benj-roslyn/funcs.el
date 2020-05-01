(defconst benj-roslyn-proj-path (concat (file-name-as-directory cos-dir) "RoslynAnalyzers"))
(defconst benj-cos-roslyn-sln-path (concat (file-name-as-directory benj-roslyn-proj-path) "RoslynAnalyzers.sln"))
(defconst benj-roslyn-cli-bin (concat (file-name-as-directory benj-roslyn-proj-path) "EntityClosureCLI/" "bin/"))
(defconst benj-roslyn-cli-name "EntityClosureCLI.exe")
(defconst idlegame-sln-path (concat idlegame-project-root "IdleGame.sln"))

;; todo
(defconst benj-roslyn-idlegame-analyzer-args
    "-x \"(Test)|(^Unity\\\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)\" -i \".*\\Assets\\.*\" -t \"Main\" --no-git")


;; TODO
(defconst benj-roslyn-proj-configs
  '((:debug . "Debug")
    (:release . "Release")))



(defun benj-roslyn--build-proj-worker (config)
  "Build cos roslyn project, CONFIG should be a string of the form 'Release' or 'Debug'."
  (benj-msbuild-sln benj-cos-roslyn-sln-path config (format "*build-roslyn-%s*" config)))


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


;; TODO
(defun benj-roslyn-run-default-idlegame ()
  "Run analzyers with default args on idlegame."
  (interactive)
  (benj-roslyn--runner-worker :release
                              (format "-s \"%s\" %s" idlegame-sln-path benj-roslyn-idlegame-analyzer-args)))


;; TODO figure out the args
(defun benj-roslyn-run-playground ()
  "Run release build on playground project."
  (interactive)
  (let (
        (buff-name "*roslyn-analzyers*")
       (process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/"))))
    (start-process
     "run-analyzers"
     buff-name
     "/usr/bin/mono"
     "/home/benj/idlegame/RoslynAnalyzers/EntityClosureCLI/bin/Release/EntityClosureCLI.exe"
     "-s" benj-cos-roslyn-sln-path
     "-t" "Playground"
     "--no-git"
     )
    (switch-to-buffer-other-window buff-name)))


(defvar sharpel-process nil)
;; TODO name this programm sharpel
(defconst sharpel-repo-root "/home/benj/repos/csharp/Sharpel/")
(defconst sharpel-proj-dir (concat sharpel-repo-root "Sharpel/"))
(defconst sharpel-sln-path (concat sharpel-repo-root "Sharpel.sln"))
(defconst sharpel-release-exe-dir (concat sharpel-proj-dir "bin/Release/netcoreapp3.0/"))
(defconst sharpel-debug-exe-dir (concat sharpel-proj-dir "bin/Debug/netcoreapp3.0/"))
(defconst sharpel-release-exe-path (concat sharpel-release-exe-dir "Sharpel.dll"))
(defconst sharpel-debug-exe-path (concat sharpel-debug-exe-dir "Sharpel.dll"))
(defconst sharpel-buff-name "*sharpel*")

(defun sharpel-start-proc ()
  "Start roslyn proc and switch to output buffer"
  (let ((default-directory sharpel-proj-dir))
   ;; (setq sharpel-process (start-process "benj-roslyn" sharpel-buff-name "mono" sharpel-debug-exe-path))
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

(defconst sharpel-command-kinds
  '((:filename . ":filename:")
    (:logsyntax . ":logsyntax:"))
  "Possible cammands send to sharpel proc.")

(defvar sharpel-last-input nil)
(defvar sharpel-last-file-send nil)

;; TODO maybe select file, default to buffer file
(defun sharpel-send-file-name-command ()
  "Send current buffer file name command to sharpel."
  (interactive) ;; interactive list form
  (sharpel--send-file-name-command buffer-file-name))

(defun sharpel--send-file-name-command (file-name)
  "Send FILE-NAME as input to sharpel."
  (setq sharpel-last-file-send file-name)
  (sharpel--runner (concat ":filename:\n" file-name "\n")))

(defun sharpel-rerun-last-file-command ()
  "Rerun last file command, if set."
  (interactive)
  (if sharpel-last-file-send (sharpel--send-file-name-command sharpel-last-file-send)
    (message "No previous file send command.")))

(defun sharpel--runner (input)
  "Ensure sharpel and run input"
  (sharpel-ensure-proc)
  (setq sharpel-last-input input)
  (process-send-string sharpel-process input)
  ;; (switch-to-buffer-other-window sharpel-buff-name)
  (pop-to-buffer sharpel-buff-name)
  (csharp-mode))

(defun sharpel-rerun-last ()
  "Rerun the last sharpel command."
  (interactive)
  (if sharpel-last-input (sharpel--runner sharpel-last-input)))
