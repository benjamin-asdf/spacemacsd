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
      (compilation-mode)
      (goto-char (point-max))))

  ;; (pop-to-buffer (benj-roslyn-tools/nuke-proc-buff))
  )

(defun benj-roslyn-tools/run-nuke-target (target)
  "Run nuke with TARGET in `benj-roslyn-tools/proj-path'."
  (benj-roslyn-tools/run-nuke "--target" target))

(defvar benj-roslyn-tools/nuke-build-proc nil)
(defun benj-roslyn-tools/run-nuke (&rest args)
  "Run nuke in `benj-roslyn-tools/proj-path'."
  (save-some-buffers)
  (let ((default-directory benj-roslyn-tools/proj-path))
    (setq benj-roslyn-tools/nuke-build-proc (nuke/runner-core args))))


(defun nuke/runner-core (&rest args)
  (prog1 (team/start-proc
    "roslyn-tools-nuke"
    (benj-roslyn-tools/nuke-proc-buff)
    "nuke"
    args)
   (benj-roslyn-tools/pop-to-nuke-buff)))


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

(defun benj-roslyn-tools/one-shot-playground (analyzer file)
  "Run one shot global ANALYZER on playground with target FILE."
  (interactive
   (benj-roslyn-tools/read-analzyer-and-file))
  (benj-roslyn-runner benj-roslyn-tools/playground-sln
                      "-t" "Playground"
                      "-g" analyzer
                      (and (not (string-empty-p file)) (list "-f" (file-name-nondirectory file)))))

(defun benj-roslyn-tools/read-analzyer-and-file ()
  (let ((analyzer (helm :sources helm-benj-roslyn-analzyers-source))
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
        (analyzer (helm :sources helm-benj-roslyn-analzyers-source)))
    (list sln file analyzer)))

(defun benj-roslyn-tools/read-file-name ()
  (read-file-name "Target file, (C-Ret to not specify target file): " nil (buffer-file-name) nil (buffer-file-name)))

(with-eval-after-load 'helm
  (defvar helm-benj-roslyn-analzyers-source
    (helm-build-sync-source "Analzyer" :candidates (benj-roslyn-tools/available-analzyer-names))))

(defun benj-roslyn-tools/run-idlegame-default (&rest args)
  "See `benj-roslyn-tools/run-idlegame'."
  (interactive)
  (benj-roslyn-tools/run-idlegame args "--no-git"))

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


(define-derived-mode
  analyzer-log-mode compilation-mode
  "analyzer-log"
  "Mode for benj roslyn tools analyzer logs."
  ;; (read-only-mode -1)

  ;; (font-lock-add-keywords)
  (setq buffer-read-only nil)

  ;; (evil-set-initial-state 'normal)

  )

;;(evil-set-initial-state 'analyzer-log-mode 'normal)

(spacemacs|define-jump-handlers analyzer-log-mode)

(add-to-list 'spacemacs-jump-handlers-analyzer-log-mode
             'benj-roslyn-tools/log-goto-warning-location
             )

(add-to-list 'auto-mode-alist `(,(format "\\%s$" benj-roslyn-tools/analzyer-log-file-ext) . analyzer-log-mode))


;; TODO in order to get the buffer in the lambda
;; I have to use make symbol


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

  (when (process-live-p benj-roslyn-tools/nuke-build-proc)
    (set-process-sentinel benj-roslyn-tools/nuke-build-proc
                          #'(lambda (proc code)
                            (pcase code
                              ("finished\n" (benj-roslyn-rerun-last))
                              (code (error "nuke build exited abnormally with code %s" code)))))
    (error "Scheduled rerun after nuke build..."))

  (let* ((file-var (benj-roslyn/diagnostics-file-name sln))
         (filter
          (benj-roslyn-tools/handle-proc
           'p
           's
           file-var
           (lambda (p s file-name)
             (benj-roslyn-tools/collect-diagnostics p s file-name)
             (benj-roslyn-tools/default-filter p s))))
         (sentinel
          (benj-roslyn-tools/handle-proc
           'p
           's
           file-var
           #'benj-roslyn-tools/write-diagnostic-out))
         (default-directory (file-name-directory sln))
         (proc (benj-start-proccess-flatten-args "run-analyzers"
                                                 benj-roslyn-tools/buff-name "dotnet" benj-roslyn-tools/cli-executable
                                                 "-s" sln args "-no-stats")))
    (set-process-filter proc filter)
    (set-process-sentinel proc sentinel))
  (pop-to-buffer benj-roslyn-tools/buff-name)
  ;; (analyzer-log-mode)
  )

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


;; obviously you should figure out, async source..?
(defun benj-roslyn-tools/rebuild-helm-analyzer-source ()
  (interactive)
  (setq helm-benj-roslyn-analzyers-source
        (helm-build-sync-source "Analzyer" :candidates (benj-roslyn-tools/available-analzyer-names))))

(defmacro benj-roslyn-tools/foreach-proj-file (&rest forms)
  "Eval form with temp file for each cs file inside `benj-roslyn-tools/proj-path'."
  (let ((dir (make-symbol "directory")))
    `(let ((,dir benj-roslyn-tools/analyzers-proj-path))
       (save-some-buffers)
       (team/--with-cs-files ,dir ,@forms))))

(defmacro benj-roslyn-tools/define-reg-repl (name doc regex replace)
  `(defun ,name ()
     (interactive)
     (benj-roslyn-tools/regex-repl-whole-proj
      ,regex
      ,replace)))

;; we could define a comment out - in thing instead
;; that would only take the string
(benj-roslyn-tools/define-reg-repl
 benj-roslyn-tools/comment-in-analyzers
 "Find DiagnosticAnalyzer attributes and comment them in."
 "^//\\[DiagnosticAnalyzer(LanguageNames.CSharp)\\]"
 "[DiagnosticAnalyzer(LanguageNames.CSharp)]")

(benj-roslyn-tools/define-reg-repl
 benj-roslyn-tools/comment-out-diagnostic-analzyers-best
 "Find DiagnosticAnalyzer attributes and comment them out."
 "^\\[DiagnosticAnalyzer(LanguageNames.CSharp)\\]"
 "//[DiagnosticAnalyzer(LanguageNames.CSharp)")

(defun benj-roslyn-tools/regex-repl-whole-proj (regex replace)
  "Not documented."
  (eval `(benj-roslyn-tools/foreach-proj-file
     (while (re-search-forward regex nil t)
       (replace-match ,replace)))))


(defun benj-roslyn-tools/log-goto-warning-location ()
  "Meant to be used in an output buffer of analyzers, jump to location of log."
  (interactive)
  (catch 'done
    (benj-rolsyn-tools/jump-line
     "^\\(/.*\\)(\\([0-9]+\\),\\([0-9]+\\)):"
     ((file (match-string-no-properties 1))
      (line (match-string-no-properties 2))
      (coll (match-string-no-properties 2))))
    (benj-rolsyn-tools/jump-line
     "^\\(/.*\\) around Line \\([0-9]+\\)"
     ((file (match-string-no-properties 1))
      (line (- (string-to-number (match-string-no-properties 2)) 1))
      (coll nil)))
    (benj-rolsyn-tools/jump-line
     "SourceFile(\\(/.*\\)\\[\\([0-9]+\\)"
     ((file (match-string-no-properties 1))
      (line (match-string-no-properties 2))
      (coll nil)))))

(defmacro benj-rolsyn-tools/jump-line (regex datas)
  "Apply REGEX to the current line.
DATAS should be a let style list that optionally sets FILE, LINE and COLL,
as appropriate.
"
  (declare (debug datas))
  `(save-excursion
    (goto-char (point-at-bol))
    (when (re-search-forward ,regex (point-at-eol) t)
      (let ,datas
        (when file
          (find-file file)
          (goto-char (point-min))
          ;; since we 0 base in the output we don't have to -1 here
          ;; leaky abstraction
          (when line (forward-line (or (and (number-or-marker-p line) line) (string-to-number line))))
          (when coll (forward-char (string-to-number coll)))
          (throw 'done t))))))


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
         (team/chsarp-snippet-insert
          "common-types-field"
          "public static CommonTypes I;"
          snippet-env)
         (team/chsarp-snippet-insert
          "common-types-get-type"
          "FrozenCollections = ImmutableHashSet.Create(MetadataSymbolComparer.I,"
          snippet-env)
         (team/chsarp-snippet-insert
          "common-types-merge-template"
          "compilation = l.compilation,"
          snippet-env))))

(defun team/chsarp-snippet-insert (snippet-name line-regex snippet-env &optional place)
  (benj-yasnippet/insert-snippet-at-place
   (yas-lookup-snippet
    snippet-name
    'csharp-mode)
   line-regex
   snippet-env
   place))

(defun benj-yasnippet/insert-snippet-at-place (snippet line-regex snippet-env &optional place)
  "Search forward for LINE-REGEX. Insert SNIPPET. SNIPPET-ENV expects a let style list. See `yas-insert-snippet'."
  (re-search-forward line-regex nil t)
  (forward-line (or place -1))
  (yas-expand-snippet
   snippet
   nil
   nil
   snippet-env))

(defun team-yas/expand-csharp-snippet (name expand-env)
  (yas-expand-snippet
   (yas-lookup-snippet
    name
    'csharp-mode)
   nil
   nil
   expand-env))






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



(provide 'team/roslyn-tools)
