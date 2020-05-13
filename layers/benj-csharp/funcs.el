(defconst benj-csharp-empty-class-template
  "\npublic class %s {\n\n}")

;; TODO move these things here
(defun benj-dotnet-add-proj-ref (&optional proj-or-sln ref-proj)
  "See `benj-dotnet--add-proj-ref'."
  (interactive (let* ((projects (benj-dotnet-fd-near-proj))
                      (def (or (and (boundp 'proj-or-sln)
                                    proj-or-sln)
                               (file-name-nondirectory (dired-file-name-at-point))))
                      (proj-or-sln (benj-dotnet--read-near-proj "Project or sln: "
                                                                projects def))
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

(defun benj-dotnet--read-near-proj (prompt projects &optional def)
  "Completing read for projects, if PROJECTS is nil, initialize with fd.
If DEF is non nil, use as default.
PROMPT can be non nil and override the default."
  (let ((projects (or projects
                      (benj-dotnet-fd-near-proj))))
    (completing-read (or prompt "Project: ") projects nil nil nil nil def)))

(defun benj-dotnet-fd-near-proj ()
  "Use fd to get near projects."
  (process-lines "fd" "-I" "-e" "csproj" "." "../"))

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
