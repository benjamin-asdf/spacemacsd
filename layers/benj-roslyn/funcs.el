;; -*- lexical-binding: t; -*-
(require 'idlegame-definitions)
(require 'team-utils)

(defconst benj-roslyn-cli-name "AnalyzerCLI.dll")
(defconst benj-roslyn-tools/proj-path (concat (file-name-as-directory cos-dir) "RoslynAnalyzers"))
(defconst benj-roslyn-tools/analyzers-proj-path (concat (file-name-as-directory benj-roslyn-tools/proj-path) "source/" "Analyzers"))
(defconst benj-roslyn-tools/analyzers-sln-path
  (concat (file-name-as-directory benj-roslyn-tools/proj-path) "RoslynAnalyzers.sln"))
(defconst benj-roslyn-tools/analyzers-test-proj-path (concat (file-name-as-directory benj-roslyn-tools/proj-path) "tests/" "Analyzers.Test"))
(defconst benj-roslyn-tools/analyzers-tests-dir (concat (file-name-as-directory benj-roslyn-tools/analyzers-test-proj-path) "Tests"))

(defconst benj-roslyn-tools/common-types-filename (concat (file-name-as-directory benj-roslyn-tools/analyzers-proj-path) "CommonTypes.cs"))


(defconst benj-roslyn-tools/cli-bin-dir (concat (file-name-as-directory benj-roslyn-tools/proj-path) "output/"))
(defconst benj-roslyn-tools/cli-executable (concat (file-name-as-directory benj-roslyn-tools/cli-bin-dir) benj-roslyn-cli-name))
(defconst benj-roslyn-tools/global-analyzers-file (concat (file-name-as-directory benj-roslyn-tools/proj-path) "src/Analyzers/GlobalAnalysis/GlobalAnalyzers.cs"))
(defconst idlegame-sln-path (concat idlegame-project-root "IdleGame.sln"))
(defconst benj-roslyn-tools/playground-proj (concat (file-name-as-directory benj-roslyn-tools/proj-path) (file-name-as-directory "RoslynPlayground")))
(defconst benj-roslyn-tools/playground-sln (concat benj-roslyn-tools/playground-proj "RoslynPlayground.sln"))
(defconst benj-roslyn-tools/playground-proj-csproj (concat benj-roslyn-tools/playground-proj "src/Playground.csproj"))
(defconst benj-roslyn-tools/banned-analyzer-proj "/home/benj/repos/BannedApiAnalyzer/source/BannedApiAnalyzer.CSharp/")

(defconst benj-roslyn-tools/analzyer-log-file-ext ".analyzer-log")
(defconst benj-roslyn-tools/analzyer-log-file (concat (temporary-file-directory) "out" benj-roslyn-tools/analzyer-log-file-ext))


(defvar benj-roslyn-tools/default-slns (list benj-roslyn-tools/playground-sln idlegame-sln-path))

(defconst benj-roslyn-tools/idlegame-args
  '("-x" "(Test)|(^Unity\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)"
    "-i" ".*Assets.*"))

(defconst benj-roslyn-tools/nuke-targets-file (concat
                                               (file-name-as-directory
                                                benj-roslyn-tools/proj-path)
                                               "build/Build.cs"))

(defvar benj-roslyn-tools/buff-name "*roslyn-analzyers*")

(defun benj-roslyn-tools/nuke-targets ()
  "Split `benj-roslyn-tools/nuke-targets-file' for nuke targets."
  (nuke/collect-targets-from-file benj-roslyn-tools/nuke-targets-file))

(defun nuke/collect-targets ()
  "Collect nuke targets from current buffer."
  (while (re-search-forward "Target \\(\\w+\\) => _ => _" nil t)
    (princ (concat (match-string 1) "\n"))))

(defun nuke/collect-targets-from-file (file)
  "Collect nuke targets from FILE. It should be a nuke build program."
  (split-string
   (with-output-to-string
     (with-temp-file file
       (insert-file-contents-literally file)
       (nuke/collect-targets)))))


(defun nuke/run-here (&optional target)
  " Run nuke with TARGET. If TARGET is not given, assumes that current buffer is a nuke program and get target from user."
  (interactive (let ((target (and buffer-file-name
                                  (completing-read
                                   "Nuke Target: "
                                   (nuke/collect-targets-from-file buffer-file-name)))))
                 (list target)))
  (nuke/runner-core "--target" target))

(defun benj-roslyn-tools/nuke-build ()
  "Build RoslynTools with nuke."
  (interactive)
  (message "buildin roslyn analyzers...")
  (benj-roslyn-prepare-build)
  (benj-roslyn-tools/run-nuke-target "Publish"))

(defun benj-roslyn-prepare-build ()
  (if
      (file-exists-p benj-roslyn-tools/analzyer-log-file)
      (team/with-file
          benj-roslyn-tools/analzyer-log-file
        (let ((inhibit-read-only t))
          (erase-buffer)))
    (write-region "" nil benj-roslyn-tools/analzyer-log-file)))

(defun benj-roslyn-build-analyzers ()
  "Only build analyzer project and put dll to output."
  (interactive)
  (require 'deferred)
  (save-some-buffers)
  (benj-roslyn-prepare-build)
  (with-dir
   benj-roslyn-tools/proj-path
   (shell-command "rm  -f output/Analyzers.dll"))
  (setf benj-roslyn-build-running t)
  (deferred:$
    (with-dir
     benj-roslyn-tools/analyzers-proj-path
     (deferred:process
       "dotnet" "build" "--configuration=Release"))
    (deferred:nextc it
      (lambda (x)
        (team/with-file benj-roslyn-tools/analzyer-log-file
          (erase-buffer)
          (->gg)
          (team/in-new-line "==================== ready ==============="))
        (with-dir
         benj-roslyn-tools/proj-path
         (copy-file
          "source/Analyzers/bin/Release/netcoreapp3.1/Analyzers.dll"
          "output/"
          'override))
        (setf benj-roslyn-build-running nil)
        (message "roslyn build success.")))))


(defun benj-roslyn-tools/nuke-proc-buff ()
  "Buffer for roslyn tools nuke proc."
  (get-buffer-create "*nuke-roslyn-tools*"))

(defun benj-roslyn-tools/run-nuke-target (target)
  "Run nuke with TARGET in `benj-roslyn-tools/proj-path'."
  (benj-roslyn-tools/run-nuke "--target" target))

(defvar benj-roslyn-tools/nuke-build-proc nil)
(defun benj-roslyn-tools/run-nuke (&rest args)
  "Run nuke in `benj-roslyn-tools/proj-path'."
  (save-some-buffers)
  (let ((default-directory benj-roslyn-tools/proj-path))
    (setq benj-roslyn-tools/nuke-build-proc
          (apply #'nuke/runner-core args))))


(defvar benj-roslyn-run-requested ())
(defvar benj-roslyn-build-running ())

(defun nuke/runner-core (&rest args)
  (require 'deferred)
  (setf benj-roslyn-build-running t)
  (deferred:$ 
    (apply
     #'deferred:process
     (append '("sh" "./build.sh") args))
    (deferred:nextc it
      (lambda (x)
        (team/with-file benj-roslyn-tools/analzyer-log-file
          (->gg)
          (team/in-new-line "==================== ready ==============="))
        (message "roslyn build success.")
        (setf benj-roslyn-build-running nil)
        (and benj-roslyn-run-requested
             benj-roslyn-last-args
             (benj-roslyn-rerun-last t))))))


(defun benj-roslyn-tools/pop-to-analyzer-log ()
  "Pop to `benj-roslyn-tools/analzyer-log-file'."
  (interactive)
  (if-let ((buff (get-buffer (file-name-nondirectory benj-roslyn-tools/analzyer-log-file))))
      (switch-to-buffer-other-window buff)
    (find-file benj-roslyn-tools/analzyer-log-file)))




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


(defun benj-roslyn-run-playground (&rest args)
  "Run release build on playground project."
  (interactive)
  (apply
   #'benj-roslyn-runner
   `(,benj-roslyn-tools/playground-sln
     ;; "-t" "Playground"
     ,@args)))

(defun benj-roslyn-tools/run-playground-sync ()
  "Run playground sync."
  (interactive)
  (benj-roslyn-runner
   benj-roslyn-tools/playground-sln
   ;; "-t" "Playground"
   "-sync"))

(defun benj-roslyn-tools/do-run-analyzers (sln target analyzer)
  "Run specific ANALYZER with SLN and TARGET."
  (interactive
   (benj-roslyn-tools/read-analzyer-runner-args))
  (benj-roslyn-runner sln (when (not (string-empty-p target)) (list "-f" (file-name-nondirectory target))) "-a" analyzer
                      (when (and (string-equal sln idlegame-sln-path) (yes-or-no-p "Selected idlegame sln. Use Default IdleGame proj inclution args? " )) (list benj-roslyn-tools/idlegame-args "--no-git"))))

(defun benj-roslyn-tools/one-shot-playground (analyzer file)
  "Run one shot global ANALYZER on playground with target FILE."
  (interactive
   (benj-roslyn-tools/read-analzyer-and-file))
  (benj-roslyn-runner benj-roslyn-tools/playground-sln
                      ;; "-t" "Playground"
                      "-g" analyzer
                      (and (not (string-empty-p file)) (list "-f" (file-name-nondirectory file)))))

(defun benj-roslyn-tools/read-analzyer-and-file ()
  (let ((analyzer (benj-roslyn-tools/read-analzyer))
        (file (benj-roslyn-tools/read-file-name)))
    (list analyzer file)))


(defun benj-rosly-tools/one-shot-idlegame (analyzer file)
  "Run one shot global ANALYZER on idegame with target FILE."
  (interactive
   (benj-roslyn-tools/read-analzyer-and-file))
  (benj-roslyn-runner idlegame-sln-path
                      benj-roslyn-tools/idlegame-args
                      "-g" analyzer
                      (and (not (string-empty-p file)) (list "-f" (file-name-nondirectory file)))))



(defun benj-roslyn-tools/read-analzyer-runner-args ()
  "Evaluates to a plist of (SLN TARGET ANALYZER)."
  (let ((sln (completing-read "Sln: " benj-roslyn-tools/default-slns))
        (file (benj-roslyn-tools/read-file-name))
        (analyzer
         (benj-roslyn-tools/read-analzyer)))
    (list sln file analyzer)))

(defun benj-roslyn-tools/read-analzyer ()
  (interactive)
  (completing-read "Analzyer: " (benj-roslyn-tools/available-analzyer-names)))


(defun benj-roslyn-tools/read-file-name ()
  (read-file-name "Target file, (C-Ret to not specify target file): " nil (buffer-file-name) nil (buffer-file-name)))

(defun benj-roslyn-tools/run-idlegame-default (&rest args)
  "See `benj-roslyn-tools/run-idlegame'."
  (interactive)
  (benj-roslyn-tools/run-idlegame args "--no-git"))

(defun benj-roslyn-tools/run-idlegame-sync (&rest args)
  "Run idlegame synced analzyers."
  (interactive)
  (benj-roslyn-tools/run-idlegame "-sync" args))

(defun benj-roslyn-tools/run-idlegame (&rest args)
  "Run release build on playground project. ARGS can be additional args."
  (interactive)
  (benj-roslyn/hack-idlegame-proj-files)
  (benj-roslyn-runner
   idlegame-sln-path
   benj-roslyn-tools/idlegame-args
   ;; "-v"
   args))

(defun benj-roslyn/hack-idlegame-proj-files ()
  "Add some mono path csproj  syntax to all project files in idlegame"
  (--each
      (directory-files idlegame-dir t "\.csproj$")
    (team/check-file
      it
      (when
          (not (re-search-forward "BaseFrameworkPathOverrideForMono" nil t))
        (->gg)
        (re-search-forward "/PropertyGroup>")
        (forward-line 1)
        (insert
         "  <PropertyGroup Condition=\"'$(OS)' == 'Unix'\">\r\n    <BaseFrameworkPathOverrideForMono Condition=\"'$(BaseFrameworkPathOverrideForMono)' == '' AND EXISTS('/Library/Frameworks/Mono.framework/Versions/Current/lib/mono')\">/Library/Frameworks/Mono.framework/Versions/Current/lib/mono</BaseFrameworkPathOverrideForMono>\r\n    <BaseFrameworkPathOverrideForMono Condition=\"'$(BaseFrameworkPathOverrideForMono)' == '' AND EXISTS('/usr/lib/mono')\">/usr/lib/mono</BaseFrameworkPathOverrideForMono>\r\n    <BaseFrameworkPathOverrideForMono Condition=\"'$(BaseFrameworkPathOverrideForMono)' == '' AND EXISTS('/usr/local/lib/mono')\">/usr/local/lib/mono</BaseFrameworkPathOverrideForMono>\r\n    <FrameworkPathOverride Condition=\"'$(BaseFrameworkPathOverrideForMono)' != ''\">$(BaseFrameworkPathOverrideForMono)/4.7.1-api</FrameworkPathOverride>\r\n    <EnableFrameworkPathOverride Condition=\"'$(BaseFrameworkPathOverrideForMono)' != ''\">true</EnableFrameworkPathOverride>\r\n    <!-- <AssemblySearchPaths Condition=\"'$(BaseFrameworkPathOverrideForMono)' != ''\">$(AsusemblySearchPaths);$(FrameworkPathOverride);$(FrameworkPathOverride)/Facades</AssemblySearchPaths> -->\r\n  </PropertyGroup>\r\n")
        t))))

(defun benj-roslyn-idlegame-sync-sym (&rest syms)
  "Invoke analyzers to search SYMS occurrances"
  (benj-roslyn-tools/run-idlegame
   "-sync"
   (concat
    "-sym="
    (s-join "," syms))
   "-dont-analyze"
   "-no-stats"))


(defvar benj-roslyn-last-args '()
  "list of sln and args of last roslyn run.")

(defun benj-roslyn-rerun-last (&optional arg)
  "Rerun `benj-roslyn-runner' with previous args."
  (interactive "P")
  (unless arg
    (benj-roslyn-build-analyzers))
  (benj-roslyn-runner (car benj-roslyn-last-args) (cdr benj-roslyn-last-args)))


(define-derived-mode
  analyzer-log-mode compilation-mode
  "analyzer-log"
  "Mode for benj roslyn tools analyzer logs."
  (setq buffer-read-only nil))

(spacemacs|define-jump-handlers analyzer-log-mode)

(add-to-list 'spacemacs-jump-handlers-analyzer-log-mode
             'benj-roslyn-tools/log-goto-warning-location
             )

(add-to-list 'auto-mode-alist `(,(format "\\%s$" benj-roslyn-tools/analzyer-log-file-ext) . analyzer-log-mode))


;; first real attempts at elisp before I knew about lexical binding :d

(defun benj-roslyn-tools/collect-diagnostics (proc string file-name)
  (when (string-match-p (format "\\(Starting Analyzer CLI with arguments:\\)\\|\\(%s\\)"
                                cos-dir)
                        string)
    (with-current-buffer
        (get-buffer-create file-name)
      (set-buffer file-name)
      (goto-char (point-max))
      (insert s))))


(defun benj-roslyn-tools/write-diagnostic-out (proc code file-name)
  (with-current-buffer
      (get-buffer-create file-name)
    (append-to-file
     (buffer-substring-no-properties
      (point-min)
      (point-max))
     nil
     file-name)
    (write-region (buffer-substring-no-properties
                   (point-min)
                   (point-max))
                  nil
                  "/tmp/last-roslyn-run")
    (kill-buffer)))


(defun benj-roslyn-runner (sln &rest args)
  "Run release analzyers with SLN and additional ARGS"
  (interactive)
  (setq args (-flatten args))
  (pushnew "-all-diagnostics" args :test 'string-equal)
  (benj-roslyn-tools/erase-analyzer-log-buff-if-exists)
  (setq benj-roslyn-last-args (list sln args))
  (when benj-roslyn-build-running
    (setf benj-roslyn-run-requested t)
    (user-error "Scheduled run after bulid"))
  (team/with-default-dir
   (file-name-directory sln)
   (apply
    #'start-process
    (append
     (list "run-analyzers"
           benj-roslyn-tools/buff-name
           "dotnet"
           ;; "mono"
           benj-roslyn-tools/cli-executable
           "-s" sln)
     args
     (list "-no-stats")))
   (pop-to-buffer benj-roslyn-tools/buff-name)))

(defun benj-roslyn-tools/handle-proc (p s file-name op)
  "Eval OP with current handle buff."
  `(lambda (,p ,s)
    (when-let ((buff (get-buffer-create ,file-name)))
      (with-current-buffer
          buff
        (funcall #',op ,p ,s ,file-name)))))

(defun benj-roslyn/diagnostics-file-name (sln)
  (concat
   (temporary-file-directory)
   (file-name-base (file-name-nondirectory sln))
   "-"
   (format-time-string "%T")
   benj-roslyn-tools/analzyer-log-file-ext))

(defun benj-roslyn-tools/erase-analyzer-log-buff-if-exists ()
  "Erase buffer of `benj-roslyn-tools/analzyer-log-file', if existent."
  (interactive)
  (when-let ((inhibit-read-only t)
             (buff (get-buffer (file-name-nondirectory benj-roslyn-tools/analzyer-log-file))))
    (with-current-buffer
        buff
      (erase-buffer)
      (save-buffer))))


(defun benj-roslyn-tools/default-filter (proc string)
  (let ((inhibit-read-only t))
    (when (buffer-live-p (process-buffer proc))
     (with-current-buffer (process-buffer proc)
       (let ((moving (= (point) (process-mark proc))))
         (save-excursion
           ;; Insert the text, advancing the process marker.
           (goto-char (process-mark proc))
           (insert string)
           (set-marker (process-mark proc) (point)))
         (if moving (goto-char (process-mark proc))))))))

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



(defconst benj-roslyn-tools/last-analzyer-id-file (concat (file-name-as-directory benj-roslyn-tools/proj-path) "last-analzyer-id"))

(defun benj-roslyn-tools/get-next-analzyer-id ()
  "Bump and return analzyer id."
  (interactive)
  (format
   "BEST%03d"
   (team/with-file
     benj-roslyn-tools/last-analzyer-id-file
     (-->
      (1+ (car (read-from-string (buffer-string))))
      (progn
        (erase-buffer)
        (insert (mkstr it))
        it)))))


(defun benj-roslyn-tools/build-banned-analzyer ()
  ""
  (interactive)
  (let ((default-directory benj-roslyn-tools/banned-analyzer-proj))
    (pop-to-buffer
     (process-buffer
      (start-process
       "build-banned-analzyer"
       "*build-banned-analzyer*"
       (concat benj-roslyn-tools/banned-analyzer-proj "scripts/build.sh"))))))


(defun benj-roslyn-tools/banned-symbols-add-warn (&optional warn)
  "Add warn level syntax to end of active lines."
  (interactive
   (let
       ((warn
         (completing-read
          "Warn Level: "
          '("Info" "Error" "Warning"))))
     (list warn)))
  (when (region-active-p)
    (goto-char (region-beginning))
    (while (re-search-forward "^.*$" (region-end) t)
      (message (match-string 0))
      ;; (let ((s (match-string 0)))
      ;;   (replace-match
      ;;    (concat
      ;;     (or
      ;;      (and
      ;;       (string-match-p "^.*;$" s) s)
      ;;      (concat s ";"))
      ;;     (or warn ";Info"))))
      )))


(defun benj-roslyn-tools/available-analzyer-names ()
  "Search `benj-roslyn-tools/analyzers-proj-path' for analzyer syntax, list names."
  (with-demoted-errors
      (let ((default-directory benj-roslyn-tools/proj-path)
         (args '(
                 "-F"
                 "-N"
                 "-I"
                 "-A" "1"
                 "-e" "[DiagnosticAnalyzer(LanguageNames.CSharp)]")))
     (cons
      "bannedapianalyzer"
      (with-temp-buffer
        (let ((status (apply 'call-process "rg" nil (current-buffer) nil args)))
          (unless (eq status 0)
	          (error "Error during finding available analyzer names %s exited with status %s" "rg" status))
          (goto-char (point-min))
          (split-string
           (with-output-to-string
             (while (re-search-forward "public class \\(\\w+\\) :"  nil t)
               (princ (concat (match-string-no-properties 1) "\n")))))))))))


(defun benj-roslyn-tools/comment-in-analyzers (&optional arg)
  (interactive "P")
  (team/each-file
   (directory-files-recursively benj-roslyn-tools/proj-path ".cs$")
   (team/while--reg
    ("\\[DiagnosticAnalyzer")
    (goto-char (point-at-bol))
    (insert "//"))))

(defmacro team/with-match-data (nums &rest body)
  "Bind match-N to each num in NUMS. Then execute BODY."
  (declare (debug body))
  (declare (indent 2))
  (if (null nums)
      `(progn ,@body)
    (let ((symb (symb 'match- (car nums))))
      `(let ((,symb (match-string-no-properties ,(car nums))))
         (team/with-match-data ,(cdr nums) ,@body)))))

(defun benj-roslyn-tools/log-goto-warning-location ()
  "Meant to be used in an output buffer of analyzers, jump to location of log."
  (interactive)
  (catch 'done
    (team/when-re-this-line
     "^\\(/.*\\)(\\([0-9]+\\),\\([0-9]+\\)):"
     (benj-roslyn-tools/line--jumper))
    (team/when-re-this-line
     "^\\(/.*\\) around Line \\([0-9]+\\)"
     (benj-roslyn-tools/line--jumper))
    (team/when-re-this-line
     "SourceFile(\\(/.*\\)\\[\\([0-9]+\\)"
     (benj-roslyn-tools/line--jumper))))

(defun benj-roslyn-tools/line--jumper ()
  (team/with-match-data
   (1 2 3)
   (team/a-when
    match-1
    (team/find-file
     it
     (+ 1 (string-to-number match-2))
     (and match-3 (string-to-number match-3)))
    (throw 'done t))))

(defun benj-roslyn-tools/make-relative-paths-from-test-dir ()
  "Make all absolute paths in the file relative to the analyzer test default dir."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "/home/benj/idlegame/RoslynAnalyzers/tests" nil t)
      (replace-match "../../../.."))))



(defun benj-roslyn-tools/goto-test-create-if-not-exists ()
  "Create test, try take word at point as default name base."
  (interactive)
  (let* ((class-name
          (concat
           (with-output-to-string
             (save-excursion
               (re-search-backward "^public class \\(\\w+\\).*Analyzer" nil t)
               (princ (match-string-no-properties 1))))
           "Tests"))
         (file-name
          (concat
           (file-name-as-directory
            benj-roslyn-tools/analyzers-tests-dir)
           class-name
           ".cs"))
         (already-exists (file-exists-p file-name)))
    (find-file file-name)
    (unless already-exists
      (yas-expand-snippet
      (yas-lookup-snippet
       "anal-test"
       'csharp-mode))
      (evil-insert-state))))


(defun benj-roslyn-tools/add-common-type (&optional name)
  "Add a new commont type of NAME"
  (interactive"sCommon type: ")
  (find-file benj-roslyn-tools/common-types-filename)
  (let* ((name-parts
          (split-string name "\\."))
         (type-name-base
          (nth-value (- (length name-parts) 1) name-parts))
         (snippet-env
          `((type-name ,name)
            (type-name-base ,type-name-base))))
    (and (goto-char (point-min))
         (team/csharp-snippet-insert
          "common-types-field"
          "public static CommonTypes I;"
          snippet-env)
         (team/csharp-snippet-insert
          "common-types-get-type"
          "FrozenCollections = ImmutableHashSet.Create(MetadataSymbolComparer.I,"
          snippet-env)
         (team/csharp-snippet-insert
          "common-types-merge-template"
          "compilation = l.compilation,"
          snippet-env))))






;; (defun benj-roslyn-tools/add-comments-to-warnings (in-file string)
;;   "Search IN-FILE for diagnostic warnings.
;; Add COMMENT-STRING to the end of all the lines."
;;   (benj-roslyn/with-collected-lines in-file #'benj-roslyn-tools/add-to-lines string))

(defun benj-roslyn-tools/disable-warnings (&optional in-file)
  "Add disable comments to all warning lines in IN-FILE."
  (interactive"f")
  (benj-roslyn/with-collected-lines
   in-file
   (// (file lines warn &rest _)
       (benj-roslyn-tools/add-to-lines
        file
        lines
        (format " // disable %s" warn)))))


(defun benj-roslyn/with-collected-lines (in-file op &rest args)
  "Collect diagnostic lines for IN-FILE and call OP with arguments:
first arg is the filename, second arg a list of lines, third arg is the warning name
and rest ARGS"
  (with-temp-buffer
    (insert-file-contents-literally in-file)
    (--each
        (benj-roslyn-tools/collect-lines-by-file)
      (apply op (append it args)))))


(defun benj-roslyn-tools/collect-lines-by-file ()
  "Search the current buffer for diagnostic lines.
Evaluate to a list lists of the form ((FILE (LINES) warn))"
  (let ((res nil))
    (save-excursion
      (goto-char (point-min))
      (let ((-file)
            (-lines)
            (-warn))
        (while
            (benj-roslyn-tools//line-data
             (unless
                 (string-equal -file file)
               (when -lines (push (list -file (nreverse -lines) -warn) res))
               (setq -lines nil)
               (setq -file file)
               (setq -warn warn))
             (when line (setq -lines (cons (string-to-number line) -lines))))))
      res)))


(defmacro benj-roslyn-tools//line-data (&rest body)
  "Set Anaphoric FILE, LINE, and WARN if success evaluate BODY and return t, else return nil"
  (declare (debug body))
  `(let* ((success (re-search-forward "^\\(/.*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning \\(\\w+\\):" nil t 1))
          (file (and success (match-string-no-properties 1)))
          (line (and success (match-string-no-properties 2)))
          (warn (and success (match-string-no-properties 4))))
     ,@body
     success))


(defun benj-roslyn-tools/add-to-lines (file lines s)
  "Add chsarp comment COMMENT-STRING to all LINES in FILE.
LINES is a list of numbers."
  (team/with-file
   file
   (--each lines
     (goto-char (point-min))
     ;; line 1 means don't forward line, zero based
     (forward-line (- it 1))
     (goto-char (point-at-eol))
     (when (looking-back "\r")
       (forward-char -1))
     (insert s))))


(defun team/crlf-p (file)
  "Test if FILE as crlf endings."
  (let ((inhibit-changing-match-data t))
    (team/with-file
     file
     (if (re-search-forward "\r\n" nil t 1)
         t
       nil))))



(defun benj-csharp-get-diagnostic-args ()
  "Get roslyn diagnostic args from last unit test run.
Put into temp buffer window."
  (interactive)
  (with-current-buffer-window
      "diagnostics-unit-tests"
      nil
      nil
    (insert
     (with-output-to-string
       (with-current-buffer
           "* Omnisharp : Unit Test Results *"
         (->gg)
         (while
             (re-search-forward
              "\\[FAILED\\] \\(.*\\)" nil t)
           (princ (match-string 0))
           (save-excursion
             (re-search-forward "Actual diagnostic:")
             (princ
              (buffer-substring
               (point)
               (progn (forward-line 3) (point)))))))))
    (->gg)
    (while (re-search-forward
            "\\.WithSpan" nil t)
      (save-excursion
        (goto-char (match-beginning 0))
        (open-line 1)
        (re-search-forward "\\.WithArguments")
        (goto-char (match-beginning 0))
        (open-line 1)
        (forward-line 1)
        (goto-char (line-end-position))
        (delete-region
         (1- (point))
         (point))))))



(defun benj-roslyn-errs (&optional re)
  (with-current-buffer
      benj-roslyn-tools/buff-name
    (->gg)
    (let ((res))
      (while (re-search-forward (concat "\\(^.+?\\)(\\([[:alnum:]]+\\),\\([[:alnum:]]+\\)):.+?" (or re "")) nil t)
        (cl-pushnew (list (match-string 1) (match-string 2) (match-string 3)) res :test #'equal))
      (nreverse res))))


;; (let ((re (regexp-opt '("BEST" "CS"))))
;;   (defun benj-roslyn-filter-best-warns (errors)
;;     (--filter (string-match-p re (flycheck-error-id it)) errors)))

;; (defun benj-roslyn-omnisharp-setup-only-best-errs ()
;;   (setq-local
;;    omnisharp-flycheck-error-filter-function
;;    #'benj-roslyn-filter-best-warns))


;; (let ((omnisharp-analzyer-copies "/tmp/CodeAnalysis/AnalyzerShadowCopies/"))
;;   (when (file-exists-p omnisharp-analzyer-copies)

;;     (delete-directory "/tmp/CodeAnalysis/AnalyzerShadowCopies/" t)))




;; (flycheck-define-generic-checker 'benj-roslyn-best-checker
;;   :start #'benj-roslyn-flycheck-start
;;   :modes '(csharp-mode)
;;   :error-filter #'omnisharp--flycheck-filter
;;   :predicate (lambda ()
;;                (s-ends-with-p "RoslynPlayground/"
;;                               (projectile-project-root))))


;; ;; TODO add something to find the solution in the working dir
;; (flycheck-define-checker
;;     benj-roslyn-best-checker
;;   "A csharp checker that only shows BEST warnings."
;;   :command
;;   ("echo"
;;    ;; "dotnet"
;;    ;; (eval benj-roslyn-tools/cli-executable)
;;    ;; "--no-git"
;;    ;; "-s"
;;    ;; (eval (benj-find-dominant-file "\.sln$"))
;;    ;; "-g"
;;    ;; (eval (benj-roslyn-tools/read-analzyer))
;;    ;; "-f"
;;    ;; (eval (file-name-nondirectory (buffer-file-name)))
;;    ;; (eval benj-roslyn-tools/playground-sln)
;;    )

;;   :working-directory  (lambda (_) (print benj-roslyn-tools/playground-proj))
;;   ;; :error-patterns (lambda)
;;   :error-parser (lambda (res)
;;                   (print "parse:" res))
;;   :modes csharp-mode
;;   :predicate (lambda () t)

;;   )

;; (--each
;;     (benj-roslyn-errs "BEST60")
;;   (team/with-file
;;       (first it)
;;     (let ((line (string-to-number (second it))))
;;       (->gg)
;;       (forward-line (- line 1))
;;       (re-search-forward "while")
;;       (forward-line -1)
;;       (goto-char (point-at-eol))
;;       (open-line 1)
;;       (forward-line 1)
;;       (insert
;;        "#pragma warning disable BEST60")
;;       (forward-line 2)
;;       (open-line 1)
;;       (insert
;;        "#pragma warning restore BEST60")
;;       )))



;; (add-to-list 'flycheck-checkers 'benj-roslyn-best-checker)





(provide 'team/roslyn-tools)
