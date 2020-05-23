(defconst benj-roslyn-proj-path (concat (file-name-as-directory cos-dir) "RoslynAnalyzers"))
(defconst benj-cos-roslyn-sln-path (concat (file-name-as-directory benj-roslyn-proj-path) "RoslynAnalyzers.sln"))
(defconst benj-roslyn-cli-bin (concat (file-name-as-directory benj-roslyn-proj-path) "EntityClosureCLI/" "bin/"))
(defconst benj-roslyn-global-analzyers-file (concat (file-name-as-directory benj-roslyn-proj-path) "Analyzers/GlobalAnalysis/GlobalAnalyzers.cs"))
(defconst benj-roslyn-cli-name "EntityClosureCLI.exe")
(defconst idlegame-sln-path (concat idlegame-project-root "IdleGame.sln"))

(defvar benj-roslyn-default-slns (list benj-cos-roslyn-sln-path idlegame-sln-path))


(defconst benj-roslyn-idlegame-analyzer-args
  '("-x" "(Test)|(^Unity\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)"
    "-i" ".*Assets.*"
    "--no-git"))


;; TODO
(defconst benj-roslyn-proj-configs
  '((:debug . "Debug")
    (:release . "Release")))

;; TODO
;; would be trivial to have some helm selection of args


(defun benj-roslyn--build-proj-worker (config)
  "Build cos roslyn project, CONFIG should be a string of the form 'Release' or 'Debug'."
  (benj-msbuild-sln benj-cos-roslyn-sln-path (concat config "Linux") (format "*build-roslyn-%s*" (concat config "Linux"))))

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

(defun benj-roslyn-do-run (sln target analyzer)
  "Run specific ANALYZER with SLN and TARGET."
  (interactive
   (benj-roslyn--read-args))
  (benj-roslyn-runner sln (when (not (string-empty-p target)) (list "-f" (file-name-nondirectory target))) "-v" "-a" analyzer
                      (when (and (string-equal sln idlegame-sln-path) (yes-or-no-p "Selected idlegame sln. Use Default IdleGame proj inclution args? " )) benj-roslyn-idlegame-analyzer-args)))


(defun benj-roslyn--read-args ()
  "Evaluates to a plist of (SLN TARGET ANALYZER)."
  (let ((sln (completing-read "Sln: " benj-roslyn-default-slns))
        (target (read-file-name "Target file, (C-Ret to not specify target file): " nil (buffer-file-name) nil (buffer-file-name)))
        (analyzer (helm :sources helm-benj-roslyn-analzyers-source)))
    (list sln target analyzer)))

(defun benj-roslyn-available-global-analzyers ()
  "Available global analzyers for roslyn project."
  (benj-roslyn--collect-analzyers benj-roslyn-global-analzyers-file))
(with-eval-after-load 'helm
  (defvar helm-benj-roslyn-analzyers-source
    (helm-build-sync-source "Analzyer" :candidates (benj-roslyn-available-global-analzyers))))

(defun benj-roslyn--collect-analzyers (file)
  "Search FILE for analyzer list pattern, return available analyzers."
  (split-string (with-output-to-string
                  (with-temp-file file
                    (insert-file-contents-literally file)
                    (while (re-search-forward ".Add<\\(\\w+\\)>()" nil t)
                      (princ (concat (match-string 1) "\n")))))))


(defun benj-roslyn-run-idlegame (&rest args)
  "Run release build on playground project. ARGS can be additional args."
  (interactive)
  (benj-roslyn-runner
   idlegame-sln-path
   benj-roslyn-idlegame-analyzer-args
   ;; "-t" "Main"
   "-v"
   "--no-git"
   args))



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


