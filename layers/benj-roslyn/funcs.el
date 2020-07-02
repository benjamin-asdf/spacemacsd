(defconst benj-roslyn-cli-name "AnalyzerCLI.dll")
(defconst benj-roslyn-tools/proj-path (concat (file-name-as-directory cos-dir) "RoslynTools"))
(defconst benj-roslyn-tools/sln-path (concat (file-name-as-directory benj-roslyn-tools/proj-path) "RoslynTools.sln"))
(defconst benj-roslyn-tools/cli-bin-dir (concat (file-name-as-directory benj-roslyn-tools/proj-path) "output/"))
(defconst benj-roslyn-tools/cli-executable (concat (file-name-as-directory benj-roslyn-tools/cli-bin-dir) benj-roslyn-cli-name))
(defconst benj-roslyn-tools/global-analyzers-file (concat (file-name-as-directory benj-roslyn-tools/proj-path) "src/Analyzers/GlobalAnalysis/GlobalAnalyzers.cs"))
(defconst idlegame-sln-path (concat idlegame-project-root "IdleGame.sln"))
(defconst benj-roslyn-tools/playground-sln (concat (file-name-as-directory cos-dir) (file-name-as-directory "RoslynPlayground") "RoslynPlayground.sln"))
(defvar benj-roslyn-tools/default-slns (list benj-roslyn-tools/sln-path idlegame-sln-path benj-roslyn-tools/playground-sln))

(defconst benj-roslyn-tools/idlegame-args
  '("-x" "(Test)|(^Unity\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)"
    "-i" ".*Assets.*"))

(defconst benj-roslyn-tools/nuke-targets-file (concat
                                               (file-name-as-directory
                                                benj-roslyn-tools/proj-path)
                                               "build/Build.cs"))

(defun benj-roslyn-tools/nuke-targets ()
  "Split `benj-roslyn-tools/nuke-targets-file' for nuke targets."
  (split-string
   (with-output-to-string
     (with-temp-file
         "/home/benj/idlegame/RoslynTools/build/Build.cs"
       (insert-file-contents-literally "/home/benj/idlegame/RoslynTools/build/Build.cs")
       (while (re-search-forward "Target \\(\\w+\\)" nil t)
         (princ (match-string-no-properties 0)))))))



(defun benj-roslyn--build-proj-worker (config)
  "Build cos roslyn project, CONFIG should be a string of the form 'Release' or 'Debug'."
  (benj-msbuild-sln benj-roslyn-tools/sln-path
                    config
                    ;;(concat config "Linux")
                    ))

(defun benj-roslyn-tools/nuke-build ()
  "Build RoslynTools with nuke."
    (benj-roslyn-tools/run-nuke "Publish"))

(defun benj-roslyn-tools/nuke-clean ()
  "Run clean target roslyn tools."
  (interactive)
    (benj-roslyn-tools/run-nuke "Clean"))

;; TODO
;; (defun benj-roslyn-tools/clean-and-build ()
;;   "Clean RoslynTools and build."
;;   (interactive)
;;   )

(defun benj-roslyn-tools/run-nuke (target)
  "Run nuke in `benj-roslyn-tools/proj-path'."
  (let ((buff-name "*nuke-roslyn-tools*")
        (default-directory benj-roslyn-tools/proj-path))
    (pop-to-buffer buff-name)
    (start-process
     "roslyn-tools-nuke"
     buff-name
     "nuke"
     "--target"
     target)))

(defun benj-roslyn-tools/run-nuke (target)
  "Run nuke in `benj-roslyn-tools/proj-path'."
  (let ((buff-name "*nuke-roslyn-tools*")
        (default-directory benj-roslyn-tools/proj-path))
    (call-process
     "nuke"
     nil
     (get-buffer-create buff-name)
     nil
     "--target"
     target)
    (pop-to-buffer buff-name)))


(defun benj-roslyn-cli-path (config)
  "Roslyn cli path for CONFIG.
Meaningfull values for CONFIG are
:release
:debug
see `benj-roslyn-proj-configs'"
  (concat benj-roslyn-tools/cli-bin-dir
          (file-name-as-directory
           (cdr (assoc config benj-roslyn-proj-configs)))
          benj-roslyn-cli-name))


(defun benj-roslyn-run-playground ()
  "Run release build on playground project."
  (interactive)
  (benj-roslyn-runner
   benj-roslyn-tools/sln-path
   "-t" "Playground"))

(defun benj-roslyn-do-run (sln target analyzer)
  "Run specific ANALYZER with SLN and TARGET."
  (interactive
   (benj-roslyn--read-args))
  (benj-roslyn-runner sln (when (not (string-empty-p target)) (list "-f" (file-name-nondirectory target))) "-v" "-a" analyzer
                      (when (and (string-equal sln idlegame-sln-path) (yes-or-no-p "Selected idlegame sln. Use Default IdleGame proj inclution args? " )) (list benj-roslyn-tools/idlegame-args "--no-git"))))


(defun benj-roslyn--read-args ()
  "Evaluates to a plist of (SLN TARGET ANALYZER)."
  (let ((sln (completing-read "Sln: " benj-roslyn-tools/default-slns))
        (target (read-file-name "Target file, (C-Ret to not specify target file): " nil (buffer-file-name) nil (buffer-file-name)))
        (analyzer (helm :sources helm-benj-roslyn-analzyers-source)))
    (list sln target analyzer)))

(defun benj-roslyn--collect-analzyers (file)
  "Search FILE for analyzer list pattern, return available analyzers."
  (when (file-exists-p file)
    (split-string (with-output-to-string
                   (with-temp-file file
                     (insert-file-contents-literally file)
                     (while (re-search-forward ".Add<\\(\\w+\\)>()" nil t)
                       (princ (concat (match-string 1) "\n"))))))))

(defun benj-roslyn-available-global-analzyers ()
  "Available global analzyers for roslyn project."
  (benj-roslyn--collect-analzyers benj-roslyn-tools/global-analyzers-file))
(with-eval-after-load 'helm
  (defvar helm-benj-roslyn-analzyers-source
    (helm-build-sync-source "Analzyer" :candidates (benj-roslyn-available-global-analzyers))))

(defun benj-roslyn/run-idlegame-default ()
  "See `benj-roslyn-run-idlegame'."
  (interactive)
  (benj-roslyn-run-idlegame "--no-git"))

(defun benj-roslyn/run-idlegame-sync ()
  "Run idlegame synced analzyers."
  (interactive)
  (benj-roslyn-run-idlegame "-sync"))

(defun benj-roslyn-run-idlegame (&rest args)
  "Run release build on playground project. ARGS can be additional args."
  (interactive)
  (benj-roslyn-runner
   idlegame-sln-path
   benj-roslyn-tools/idlegame-args
   "-v"
   args))

(defun benj-roslyn-tools/clean-and-publish ()
  "Run nuke clean and publish."



  )


;; "-a" "StartupMethodAnalyzer" "-startup"
;; "-e" "UNITY_EDITOR"
;; "-p" "UNITY_IOS"

(defvar benj-roslyn-last-args '()
  "list of sln and args of last roslyn run.")

(defun benj-roslyn-rerun-last ()
  "Rerun `benj-roslyn-runner' with previous args."
  (interactive)
  (if benj-roslyn-last-args
      (benj-roslyn-runner (car benj-roslyn-last-args) (cdr benj-roslyn-last-args))))

(defun benj-roslyn-runner (sln &rest args)
  "Run release analzyers with SLN and additional ARGS"
  (interactive)
  (setq benj-roslyn-last-args (list sln args))
  (let ((default-directory (file-name-directory sln))
        (buff-name "*roslyn-analzyers*")
        (process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/"))))
    (benj-start-proccess-flatten-args
     "run-analyzers"
     buff-name
     ;; "/usr/bin/mono"
     benj-roslyn-tools/cli-executable
     "-s" sln
     args)
    (pop-to-buffer buff-name)))

(defun benj-roslyn//run-closure ()
  "TEMP run closure."
  (interactive)
  (let ((default-directory benj-roslyn-tools/proj-path))
    (pop-to-buffer
     (process-buffer
      (start-process "roslyn-runner" "*roslyn-analzyers*"
                     (if (yes-or-no-p "idlgame?")
                         "/home/benj/idlegame/RoslynTools/run-idlgame.sh"
                       "/home/benj/idlegame/RoslynTools/run-closure.sh"))))))

(defun benj-roslyn-runner (sln &rest args)
  "Run release analzyers with SLN and additional ARGS"
  (interactive)
  (setq benj-roslyn-last-args (list sln args))
  (let ((default-directory (file-name-directory sln))
        (buff-name "*roslyn-analzyers*")
        (process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/")))
        )
    (benj-start-proccess-flatten-args
     "run-analyzers"
     buff-name
     "/usr/bin/mono"
     ;; "dotnet"
     benj-roslyn-tools/cli-executable
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


