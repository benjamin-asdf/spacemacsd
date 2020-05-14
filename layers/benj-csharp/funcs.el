(defconst benj-omnisharp-repo-root "~/repos/omnisharp-roslyn/")
(defconst benj-omnisharp-server-executable (concat benj-omnisharp-repo-root "artifacts/publish/OmniSharp.Stdio.Driver/mono/OmniSharp.exe"))
(defconst benj-csharp-empty-class-template
  "\npublic class %s {\n\n}")

(defconst benj-chsarp-gitignore-template "
obj/
bin/
")

(defun benj-dotnet-add-default-gitignore ()
  "Add default gitignore, to currently visited dir."
  (interactive)
  (let ((curr-file-name (or (dired-file-name-at-point) buffer-file-name)))
    (if curr-file-name
        (append-to-file benj-chsarp-gitignore-template nil (concat (file-name-directory curr-file-name) ".gitignore"))
      (message "buffer not visiting a file"))))

;; TODO move these things here
(defun benj-dotnet-add-proj-ref (&optional proj-or-sln ref-proj)
  "See `benj-dotnet--add-proj-ref'."
  (interactive (let* ((projects (benj-near-proj-and-slns))
                      (proj-or-sln (benj-dotnet--read-proj-or-sln-def-to-point))
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
  "Evaluates to true if FILE is the path to a sln"
  (string-equal (file-name-extension file) "sln"))

(defun benj-dotnet-proj-p (file)
  "True if FILE is the path to a csproj."
  (string-equal (file-name-extension file) "csproj"))

(defun benj-dotnet-proj-or-sln-p (file)
  "See `benj-dotnet-sln-p' and `benj-dotnet-proj-p'"
  (or (benj-dotnet-proj-p file) (benj-dotnet-sln-p file)))

(defun benj-dotnet--read-proj-or-sln-def-to-point (projects)
  "Do completing red for near projects, see `benj-dotnet--read-near-proj'.
Use `dired-file-name-at-point' as default value.
If PROJECTS is nil initialize new projects using `benj-near-proj-and-slns'"
  (let ((def (file-name-nondirectory (dired-file-name-at-point))))
    (benj-dotnet--read-near-proj "Project or sln: "
                                 projects (and (benj-dotnet-proj-or-sln-p def) def))))

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
  (interactive (let ((sln-path (or (and (boundp 'sln-path) sln-path)
                                   (benj-dotnet--read-proj-or-sln-def-to-point nil)))
                     (config (or (and (boundp 'config) config)
                                 (completing-read "config: "
                                                  '("Release" "Debug")))))
                 (list sln-path config)))
  (benj-msbuild-sln sln-path config))

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
