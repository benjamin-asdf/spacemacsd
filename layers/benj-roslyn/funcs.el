(defconst benj-roslyn-cli-name "AnalyzerCLI.dll")
(defconst benj-roslyn-tools/proj-path (concat (file-name-as-directory cos-dir) "RoslynAnalyzers"))
(defconst benj-roslyn-tools/analyzers-proj-path (concat (file-name-as-directory benj-roslyn-tools/proj-path) "source/" "Analyzers"))
(defconst benj-roslyn-tools/analyzers-sln-path
  (concat (file-name-as-directory benj-roslyn-tools/proj-path) "RoslynAnalyzers.sln"))


(defconst benj-roslyn-tools/cli-bin-dir (concat (file-name-as-directory benj-roslyn-tools/proj-path) "output/"))
(defconst benj-roslyn-tools/cli-executable (concat (file-name-as-directory benj-roslyn-tools/cli-bin-dir) benj-roslyn-cli-name))
(defconst benj-roslyn-tools/global-analyzers-file (concat (file-name-as-directory benj-roslyn-tools/proj-path) "src/Analyzers/GlobalAnalysis/GlobalAnalyzers.cs"))
(defconst idlegame-sln-path (concat idlegame-project-root "IdleGame.sln"))
(defconst benj-roslyn-tools/playground-proj (concat (file-name-as-directory cos-dir) (file-name-as-directory "RoslynPlayground")))
(defconst benj-roslyn-tools/playground-sln (concat benj-roslyn-tools/playground-proj "RoslynPlayground.sln"))
(defconst benj-roslyn-tools/playground-proj-csproj (concat benj-roslyn-tools/playground-proj "src/Playground.csproj"))
(defconst benj-roslyn-tools/banned-analyzer-proj "/home/benj/repos/BannedApiAnalyzer/source/BannedApiAnalyzer.CSharp/")

(defconst benj-roslyn-tools/analzyer-log-file (concat (temporary-file-directory) "analyzer-log"))
(defvar benj-roslyn-tools/default-slns (list benj-roslyn-tools/playground-sln idlegame-sln-path))

(defconst benj-roslyn-tools/idlegame-args
  '("-x" "(Test)|(^Unity\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)"
    "-i" ".*Assets.*"))

(defconst benj-roslyn-tools/nuke-targets-file (concat
                                               (file-name-as-directory
                                                benj-roslyn-tools/proj-path)
                                               "build/Build.cs"))

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
  (unless (file-exists-p benj-roslyn-tools/analzyer-log-file)
    (write-region "" nil benj-roslyn-tools/analzyer-log-file))
  ;; (when-let ((buff (get-buffer (file-name-nondirectory benj-roslyn-tools/analzyer-log-file))))
  ;;   (switch-to-buffer-other-frame buff)
  ;;   (erase-buffer)
  ;;   (save-buffer))
  ;; (find-file-other-window benj-roslyn-tools/analzyer-log-file)
  (benj-roslyn-tools/run-nuke-target "Publish"))


(defun benj-roslyn-tools/nuke-clean ()
  "Run clean target roslyn tools."
  (interactive)
  (benj-roslyn-tools/run-nuke-target "Clean"))

(defun benj-roslyn-tools/nuke-proc-buff ()
  "Buffer for roslyn tools nuke proc."
  (get-buffer-create "*nuke-roslyn-tools*"))

(defun benj-roslyn-tools/pop-to-nuke-buff ()
  "Pop to buff other frame returned by `benj-roslyn-tools/nuke-proc-buff'."
  (interactive)
  (let ((buff (benj-roslyn-tools/nuke-proc-buff)))
    (unless (equal buff (current-buffer))
      (switch-to-buffer-other-frame buff)
      (goto-char (point-max))))

  ;; (pop-to-buffer (benj-roslyn-tools/nuke-proc-buff))
  )

;; TODO
;; (defun benj-roslyn-tools/clean-and-build ()
;;   "Clean RoslynTools and build."
;;   (interactive)
;;   )

(defun benj-roslyn-tools/run-nuke-target (target)
  "Run nuke with TARGET in `benj-roslyn-tools/proj-path'."
  (benj-roslyn-tools/run-nuke "--target" target))

(defun benj-roslyn-tools/run-nuke (&rest args)
  "Run nuke in `benj-roslyn-tools/proj-path'."
  (save-some-buffers)
  (let ((default-directory benj-roslyn-tools/proj-path))
    (nuke/runner-core args)))

(defun nuke/runner-core (&rest args)
  (benj-start-proccess-flatten-args
   "roslyn-tools-nuke"
   (benj-roslyn-tools/nuke-proc-buff)
   "nuke"
   args)
  (benj-roslyn-tools/pop-to-nuke-buff))


;; TODO with callback
;; (defun benj-roslyn-tools/run-nuke (callback &rest args)
;;   "Run nuke in `benj-roslyn-tools/proj-path'.
;; If CALLBACK non nil, do not immediatly pop to buffer.
;; if CALLBACK is a symbol bound to a function without parameters, it get's called after the process finishes.
;; "
;;   ;; todo callback could have the value 'pop-to-nuke-buff
;;   (let* ((default-directory benj-roslyn-tools/proj-path)
;;          (proc (benj-start-proccess-flatten-args
;;                 "roslyn-tools-nuke"
;;                 (benj-roslyn-tools/nuke-proc-buff)
;;                 "nuke"
;;                 args)))
;;     (or (and
;;          (fboundp callback)
;;          (set-process-sentinel
;;           proc
;;           (lambda (proc evnt)
;;             (apply callback))))
;;         (benj-roslyn-tools/pop-to-nuke-buff))))

;; (benj-roslyn-tools/run-nuke #'benj-roslyn-tools/pop-to-nuke-buff "--target" "Publish")


;; (defun benj-roslyn-tools/run-nuke (target)
;;   "Run nuke in `benj-roslyn-tools/proj-path'."
;;   (let ((buff-name "*nuke-roslyn-tools*")
;;         (default-directory benj-roslyn-tools/proj-path))
;;     (call-process
;;      "nuke"
;;      nil
;;      (get-buffer-create buff-name)
;;      nil
;;      "--target"
;;      target)
;;     (pop-to-buffer buff-name)))

(defun benj-roslyn-tools/pop-to-analyzer-log ()
  "Pop to `benj-roslyn-tools/analzyer-log-file'."
  (interactive)
  (cond
   ((when-let ((buff (get-buffer (file-name-nondirectory benj-roslyn-tools/analzyer-log-file))))
      (switch-to-buffer-other-window buff)))
   ((file-exists-p benj-roslyn-tools/analzyer-log-file)
    (find-file benj-roslyn-tools/analzyer-log-file))
   (t (message "There is no current analyzer log file."))))

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
   benj-roslyn-tools/playground-sln
   "-t" "Playground")
  )

(defun benj-roslyn-tools/run-playground-sync ()
  "Run playground sync."
  (interactive)
  (benj-roslyn-runner
   benj-roslyn-tools/playground-sln
   "-t" "Playground"
   "-sync"))

(defun benj-roslyn-tools/do-run-analyzers (sln target analyzer)
  "Run specific ANALYZER with SLN and TARGET."
  (interactive
   (benj-roslyn-tools/read-analzyer-runner-args))
  (benj-roslyn-runner sln (when (not (string-empty-p target)) (list "-f" (file-name-nondirectory target))) "-a" analyzer
                      (when (and (string-equal sln idlegame-sln-path) (yes-or-no-p "Selected idlegame sln. Use Default IdleGame proj inclution args? " )) (list benj-roslyn-tools/idlegame-args "--no-git"))))

(defun benj-roslyn-tools/run-test-file (analyzer file)
  "Run ANALYZER with test FILE."
  (interactive
   (let ((analyzer (helm :sources helm-benj-roslyn-analzyers-source))
         (file (benj-roslyn-tools/read-file-name)))
     (list analyzer file)))
  (benj-roslyn-runner benj-roslyn-tools/analyzers-sln-path
                      "-t" "Analyzers.CSharp.UnitTests"
                      "-a" analyzer
                      "-f" (file-name-nondirectory file)))

(defun benj-roslyn-tools/one-shot-playground (analyzer file)
  "Run one shot global ANALYZER on playground with target FILE."
  (interactive
   (let ((analyzer (helm :sources helm-benj-roslyn-analzyers-source))
         (file (benj-roslyn-tools/read-file-name)))
     (list analyzer file)))
  (benj-roslyn-runner benj-roslyn-tools/playground-sln
                      "-t" "Playground"
                      "-g" analyzer
                      "-f" (file-name-nondirectory file)
                      )
  )


(defun benj-roslyn-tools/read-analzyer-runner-args ()
  "Evaluates to a plist of (SLN TARGET ANALYZER)."
  (let ((sln (completing-read "Sln: " benj-roslyn-tools/default-slns))
        (file (benj-roslyn-tools/read-file-name))
        (analyzer (helm :sources helm-benj-roslyn-analzyers-source)))
    (list sln file analyzer)))

(defun benj-roslyn-tools/read-file-name ()
  (read-file-name "Target file, (C-Ret to not specify target file): " nil (buffer-file-name) nil (buffer-file-name)))

(with-eval-after-load 'helm
  (defvar helm-benj-roslyn-analzyers-source
    (helm-build-sync-source "Analzyer" :candidates (benj-roslyn-tools/available-analzyer-names))))

(defun benj-roslyn-tools/run-idlegame-default ()
  "See `benj-roslyn-tools/run-idlegame'."
  (interactive)
  (benj-roslyn-tools/run-idlegame "--no-git"))

(defun benj-roslyn-tools/run-idlegame-sync ()
  "Run idlegame synced analzyers."
  (interactive)
  (benj-roslyn-tools/run-idlegame "-sync"))

(defun benj-roslyn-tools/run-idlegame (&rest args)
  "Run release build on playground project. ARGS can be additional args."
  (interactive)
  (benj-roslyn-runner
   idlegame-sln-path
   benj-roslyn-tools/idlegame-args
   ;; "-v"
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

;; (mapconcat (format "`%s`") )

;; (defun benj-roslyn-runner (sln &rest args)
;;   "Run release analzyers with SLN and additional ARGS"
;;   (interactive)
;;   (setq benj-roslyn-last-args (list sln args))

;;     (benj-roslyn-tools/run-nuke
;;      (format "Run-Analyzers --application-arguments '`%s`'" (mapconcat ))

;;      (concat )
;;      (concat (-flatten (list "" "-s" sln args)))
;;      )





;;   ;; (let ((default-directory (file-name-directory sln))
;;   ;;       (buff-name "*roslyn-analzyers*"))
;;   ;;   (benj-start-proccess-flatten-args
;;   ;;    "run-analyzers"
;;   ;;    buff-name
;;   ;;    ;; "/usr/bin/mono"
;;   ;;    benj-roslyn-tools/cli-executable
;;   ;;    "-s" sln
;;   ;;    args)
;;   ;;   (pop-to-buffer buff-name))
;;   )


;; (defun benj-roslyn-tools/clear-runner-buff ()
;;   "Clear runner buff."
;;   (get-buffer-create )
;;   (erase-buffer )
;;   )


(defun benj-roslyn-runner (sln &rest args)
  "Run release analzyers with SLN and additional ARGS"
  (interactive)
  (benj-roslyn-tools/erase-analyzer-log-buff-if-exists)
  (setq benj-roslyn-last-args (list sln args))
  (let ((default-directory (file-name-directory sln))
        (buff-name "*roslyn-analzyers*")
        (process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/")))
        )
    (benj-start-proccess-flatten-args
     "run-analyzers"
     buff-name
     "dotnet"
     benj-roslyn-tools/cli-executable
     "-s" sln
     args
     "-no-stats"
     )
    (pop-to-buffer buff-name)))


(defun benj-roslyn-tools/erase-analyzer-log-buff-if-exists ()
  "Erase buffer of `benj-roslyn-tools/analzyer-log-file', if existent."
  (interactive)
  (when-let ((buff (get-buffer (file-name-nondirectory benj-roslyn-tools/analzyer-log-file))))
    (with-current-buffer
        buff
      (erase-buffer)
      (save-buffer))))


;; could run with nuke,
;; atm there we dont find msbuild



;; (defun benj-roslyn-runner (sln &rest args)
;;   "Run release analzyers with SLN and additional ARGS"
;;   (interactive)
;;   (setq benj-roslyn-last-args (list sln args))
;;   (let ((process-environment (append process-environment (list "CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/Current/bin/"))))
;;     (benj-roslyn-tools/run-nuke
;;      "--target" "BuildAndRunAnalyzers"
;;      "--application-arguments"
;;      (format  "`-s %s %s`" sln (mapconcat 'identity (-flatten args) " "))
;;      )))



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
  "Bump current analzyer id and return next id."
  (interactive)
  (let ((next (number-to-string (+ (string-to-number (car (benj-read-lines benj-roslyn-tools/last-analzyer-id-file))) 1))))
    (write-region next nil benj-roslyn-tools/last-analzyer-id-file)
    (kill-new next)))


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
  (let ((default-directory benj-roslyn-tools/proj-path)
        (args '(
                "-F"
                "-N"
                "-I"
                "-A" "1"
                "-e" "[DiagnosticAnalyzer(LanguageNames.CSharp)]")))
    (with-temp-buffer
      (let ((status (apply 'call-process "rg" nil (current-buffer) nil args)))
        (unless (eq status 0)
	        (error "%s exited with status %s" "rg" status))
        (goto-char (point-min))
        (split-string
         (with-output-to-string
           (while (re-search-forward "public class \\(\\w+\\) :"  nil t)
             (princ (concat (match-string-no-properties 1) "\n")))))))))



;; obviously you should figure out, async source..?
(defun benj-roslyn-tools/rebuild-helm-analyzer-source ()
  (interactive)
  (setq helm-benj-roslyn-analzyers-source
        (helm-build-sync-source "Analzyer" :candidates (benj-roslyn-tools/available-analzyer-names))))

(defun benj-roslyn-tools/comment-out-diagnostic-analzyers (&optional dir)
  "Find DiagnosticAnalyzer attributes DIR and comment them out."
  (interactive)
  (let ((dir (or dir benj-roslyn-tools/analyzers-proj-path)))
    (save-some-buffers)
    (--map
     (with-temp-file it
       (insert-file-contents-literally it)
       (while (re-search-forward "^\\[DiagnosticAnalyzer(LanguageNames.CSharp)\\]" nil t)
         (replace-match (concat "//" (match-string-no-properties 0)))))
     (directory-files-recursively dir "\\.cs"))))

(defun benj-roslyn-tools/log-goto-warning-location ()
  "Meant to be used in an output buffer of analyzers, jump to location of log."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (when (or
           (re-search-forward "SourceFile(\\(/.*\\)\\[\\([0-9]+\\)" (point-at-eol) t)
           (re-search-forward "^\\(/.*\\)(\\([0-9]+\\),\\([0-9]+\\)):" (point-at-eol) t)
           (re-search-forward "^\\(/.*\\) around Line \\([0-9]+\\)" (point-at-eol) t))
      (let* ((file (match-string-no-properties 1))
            (line (match-string-no-properties 2))
            ;; FIXME not like this
            (coll (match-string-no-properties (if line 3 2))))
        (find-file file)
        (goto-char (point-min))
        ;; since we 0 base in the output we don't have to -1 here
        (when line (forward-line (string-to-number line)))
        (when coll (forward-char (string-to-number coll)))))))
