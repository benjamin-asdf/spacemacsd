(defconst benj/omnisharp-repo-root "~/repos/omnisharp-roslyn/")
(defconst benj/omnisharp-server-executable (concat benj/omnisharp-repo-root "artifacts/scripts/OmniSharp.Stdio"))

(defconst benj-csharp-empty-class-template
  "\npublic class %s {\n\n}")

(defconst benj-chsarp-gitignore-template "
obj/
bin/
")

(defvar benj-chsarp-working-projects '()
  "List of currently worked on projects for quick use of compile funcs etc.")

(defun benj-csharp-push-working-proj (file)
  "Add PROJ to `benj-chsarp-working-projects'"
  (unless (member file benj-chsarp-working-projects) (push file benj-chsarp-working-projects)))

(defun benj-dotnet-add-default-gitignore ()
  "Add default gitignore, to currently visited dir."
  (interactive)
  (let ((curr-file-name (or (dired-file-name-at-point) buffer-file-name)))
    (if curr-file-name
        (append-to-file benj-chsarp-gitignore-template nil (concat (file-name-directory curr-file-name) ".gitignore"))
      (message "buffer not visiting a file"))))

;; TODO move these things here
(defun benj-dotnet-add-proj-ref (&optional proj-or-sln ref-proj)
  "Add REF-PROJ to PROJ-OR-SLN."
  (interactive (let* ((projects (benj-near-proj-and-slns))
                      (proj-or-sln (benj-dotnet--read-proj-or-sln-def-to-point projects))
                      (ref-proj (or (and (boundp 'ref-proj)
                                         ref-proj)
                                    (benj-dotnet--read-near-proj "Add project ref: "
                                                                 projects))))
                 (list proj-or-sln ref-proj)))
  (benj-shell-command-and-pop-to-buff (format (or (and (benj-dotnet-sln-p proj-or-sln)
                                                       "dotnet sln %s add %s")
                                                  "dotnet add %s reference %s")
                                              proj-or-sln
                                              ref-proj)))

(defun benj-dotnet-sln-p (file)
  "Evaluate to true if FILE is the path to a sln."
  (string-equal (file-name-extension file) "sln"))

(defun benj-dotnet-proj-p (file)
  "True if FILE is the path to a csproj."
  (string-equal (file-name-extension file) "csproj"))

(defun benj-dotnet-proj-or-sln-p (file)
  "Is FILE a sln or proj? See `benj-dotnet-sln-p' and `benj-dotnet-proj-p'."
  (or (benj-dotnet-proj-p file) (benj-dotnet-sln-p file)))

(defun benj-dotnet--read-proj-or-sln-def-to-point (projects)
  "Do completing red for near projects, see `benj-dotnet--read-near-proj'.
Use `dired-file-name-at-point' as default value.
If PROJECTS is nil initialize new projects using `benj-near-proj-and-slns'"
  (let ((def (file-name-nondirectory (or (dired-file-name-at-point) (buffer-file-name)))))
    (benj-dotnet--read-near-proj "Project or sln: "
                                 (or (and projects (-union projects benj-chsarp-working-projects)) benj-chsarp-working-projects) (and (benj-dotnet-proj-or-sln-p def) def))))

(defun benj-dotnet--read-near-proj (prompt projects &optional def)
  "Completing read for projects, if PROJECTS is nil, initialize with fd.
If DEF is non nil, use as default.
PROMPT can be non nil and override the default."
  (let ((projects (or projects
                      (benj-near-proj-and-slns))))
    (completing-read (or prompt "Project: ") projects nil nil nil nil def)))

(defun benj-near-proj-and-slns ()
  "Use fd to get near projects. Also search open dired buffers for csproj files."
  (-union (process-lines "fd" "-I" "-e" "csproj" "-e" "sln" "." "../")
          (--remove (null it)
                    (--map (and (file-exists-p (car it))
                                (directory-files (car it)
                                                 t
                                                 "\.\\(?:csproj\\|sln\\)$"))
                           dired-buffers))))

(defun benj-do-msbuild (sln-path config)
  "Build SLN-PATH with CONFIG.
Args can be ommitted and queried by user."
  (interactive (let* ((sln-path (or (and (boundp 'sln-path) sln-path)
                                   (benj-dotnet--read-proj-or-sln-def-to-point nil)))
                     (config (or (and (boundp 'config) config)
                                 (benj-dotnet-read-build-config sln-path))))
                 (list sln-path config)))

  (benj-msbuild-sln sln-path config))


;; TODO actually read available configs from sln file
(defun benj-dotnet-read-build-config (proj-or-sln)
  "Completing read from a list of configs."
  (completing-read "config: " (benj-dotnet-proj-configs proj-or-sln)))

(defun benj-dotnet-proj-configs (proj-file)
  "Read available configurations from PROJ-FILE."
  (if (benj-dotnet-proj-p proj-file) (with-temp-buffer
     (insert-file-contents-literally proj-file)
     (progn (re-search-forward "<Configurations>\\(.*\\)</Configurations>" nil t)
            (split-string (match-string-no-properties 1) ";")))
    (progn
      ;; TODO
      (message "Reading config fromo sln not supported yet.")
      '("Release" "Debug" "ReleaseLinux" "DebugLinux"))))


(defun benj-dotnet--add-proj-ref (proj ref-proj)
  "Add REF-PROJ reference to PROJ.")

(defun benj-shell-command-and-pop-to-buff (command)
  "Run `shell-command' with COMMAND and pop to buffer."
  (shell-command command)
  (pop-to-buffer "*Shell Command Output*"))

(defun benj-dotnet--proj-read ()
  "Prompt the user for a project, default to point.")

(defun benj-csharp-file-and-class (&optional name)
  "Create a new csharp FILE with empty class defnition."
  (interactive "FCreate charp class: ")
  (find-file (or (and (string-equal "cs"
                                    (file-name-extension name))
                      name)
                 (concat name ".cs")))
  (insert (format benj-csharp-empty-class-template
                  (file-name-base)))
  (forward-line -1)
  (indent-according-to-mode)
  (evil-insert-state))


(defun benj-msbuild-sln (sln-path config &rest args)
  "Build sln at SLN-PATH using mono msbuild. CONFIG is a string passed as /p:Configuration=
usually something like 'Release'.
Optional BUFF-NAME to put proc output in a custom buffer. "
  (benj-csharp-push-working-proj sln-path)
  (let ((buff-name (or (and (boundp 'buffer-name) buff-name) (format "*msbuild-%s*" config))))
    (benj-start-proccess-flatten-args "benj-msbuild" buff-name "msbuild" sln-path (format "/p:Configuration=%s" config args))
    (switch-to-buffer-other-window buff-name)))


(defun benj-chsarp-msbuild-this-proj ()
  "Try ms build sln at `projectile-project-root'"
  (interactive)
  (let ((maybe-sln (car (directory-files (concat (projectile-project-root)) t "\.\\(?:csproj\\|sln\\)$"))))
    (if (and maybe-sln (file-exists-p maybe-sln))
        (benj-msbuild-sln maybe-sln (benj-dotnet-read-build-config)))))

(defun benj-do-msbuild-dwim()
  "If there are any recent projects, Ask to build."
  (interactive)
  (benj-msbuild-sln (benj-dotnet--read-near-proj nil nil) "Release" ))

(defun benj-dotnet-run()
  "Run dotnet run is current dir, open output in a dedicated buffer."
  (interactive)
  (save-some-buffers)
  (benj-process-other-window "dotnet-run" "*dotnet-run*" "dotnet" "run"))


(defun benj-dotnet-sln-project-list (sln)
  "Get proj list from SLN."
  (split-string
   (with-output-to-string
     (with-temp-buffer
       (insert-file-contents-literally sln)
       (while (re-search-forward "Project(\"{.*}\") = \"\\(\\w+\\)\"" nil t)
         (princ (format "%s\0" (match-string 1))))))
   "\0"))



(defun benj-csharp/replace-wl-with-debug ()
  "Replace console write line with debug.log in buff."
  (interactive)
  (benj//replace-match-buff "Console\\.WriteLine" "UnityEngine.Debug.Log"))

(defun benj//replace-match-buff (match newtext)
  "Repalce MATCH with NEXTEXT in buff."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward match nil t) (replace-match newtext))))














;; omnisharp

(defun benj/omnisharp-start-near-proj ()
  "Start with sln in projectile project root. Ask for sln if there is none.
Depends on mono and an omnisharp build at `benj/omnisharp-server-executable'."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (sln-path (or
                   (car (directory-files (projectile-project-root) t ".sln$"))
                   (read-file-name "Sln file to use: " ))))
    (unless (benj-dotnet-sln-p sln-path)
      (user-error "Invalid sln path %s" sln-path))
    (benj/omnisharp--do-server-start sln-path benj/omnisharp-server-executable)))


(defun benj/omnisharp--do-server-start (sln server-executable-path &rest args)
  "Start omnisharp process SERVER-EXECUTABLE-PATH with SLN and ARGS."
    (message "benj-omnisharp: starting server with sln: \"%s\"" sln)

   (omnisharp--log-reset)
   (omnisharp--log (format "starting server with sln \"%s\"" sln))
   (omnisharp--log (format "Using server executbale on %s" server-executable-path))

   ;; Save all csharp buffers to ensure the server is in sync"
   ;; (save-some-buffers t (lambda () (and (buffer-file-name) (string-equal (file-name-extension (buffer-file-name)) "cs"))))

   (setq omnisharp--last-project-path (file-name-directory sln))

   ;; this can be set by omnisharp-reload-solution to t
   (setq omnisharp--restart-server-on-stop nil)

   (setq omnisharp--server-info
         (make-omnisharp--server-info
          ;; use a pipe for the connection instead of a pty
          (let* ((process-connection-type nil)
                 (default-directory (expand-file-name (file-name-directory sln)))
                 (omnisharp-process (start-process
                                     "BenjOmniServer" ; process name
                                     "BenjOmniServer" ; buffer name
                                     ;; "mono" ;; if I use the script in artifacts/scripts/ that already uses mono
                                     server-executable-path
                                     "--encoding" "utf-8"
                                     "--stdio"
                                     "-v"
                                     "-s"
                                     "-p" "8083"
                                     sln
                                     ;; Options:
                                     ;; -? | -h | --help           Show help information
                                     ;; -s | --source              Solution or directory for OmniSharp to point at (defaults to current directory).
                                     ;; -l | --loglevel            Level of logging (defaults to 'Information').
                                     ;; -v | --verbose             Explicitly set 'Debug' log level.
                                     ;; -hpid | --hostPID          Host process ID.
                                     ;; -z | --zero-based-indices  Use zero based indices in request/responses (defaults to 'false').
                                     ;; -pl | --plugin             Plugin name(s).
                                     ;; -d | --debug               Wait for debugger to attach
                                     ;; -p | --port                OmniSharp port (defaults to 2000).
                                     ;; -i | --interface           Server interface address (defaults to 'localhost').

                                     )))
            (buffer-disable-undo (process-buffer omnisharp-process))
            (set-process-query-on-exit-flag omnisharp-process nil)
            (set-process-filter omnisharp-process 'omnisharp--handle-server-message)
            (set-process-sentinel omnisharp-process
                                  (lambda (process event)
                                    (when (memq (process-status process) '(exit signal))
                                      (message "benj omnisharp: server has been terminated")
                                      (setq omnisharp--server-info nil))))
            omnisharp-process)
          sln)))


;; I don't want that
(advice-add 'omnisharp--attempt-to-start-server-for-buffer :around (lambda (orig-fun)))


(setq omnisharp-host "http://localhost:8083/")














;; hack to remove some errors
;; but atm the omnisharp get diagnostics is timing out in non-small files, so yea

;; (advice-add 'omnisharp--flycheck-start  :around #'benj-omnisharp/flycheck-start-adviced)

;;See `omnisharp--flycheck-start'.
(defun benj-omnisharp/flycheck-start-adviced (orig-func checker callback)
  "Start an OmniSharp syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  ;; Put the current buffer into the closure environment so that we have access
  ;; to it later.
  (let ((buffer (current-buffer)))
    (omnisharp--send-command-to-server
     "codecheck"
     (omnisharp--get-request-object)
     (lambda (response)
       (let ((errors (omnisharp--flycheck-error-parser response checker buffer)))
         (funcall callback 'finished (delq nil (benj-omnisharp/filter-flycheck-errors errors))))))))

(defun benj-omnisharp/filter-flycheck-errors (errors)
  "Hack to filter omnisharp flycheck errors."
  (--filter
   (string-prefix-p
    "Expression value is never used"
    (flycheck-error-message it))
   errors))
