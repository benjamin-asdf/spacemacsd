(defconst bunel-handle-path "/tmp/bunel-handles")
(defconst bunel-idlegame-projects '("IdleGame" "IdleGameSymbolicLink" "IdleGameSymbolicLink-Extra"))
(defconst bunel-default-unity-project "IdleGame")

(defconst bunel-method-names
  '((bunel-refresh . "refresh")))

(defun bunel-create-handle-file (project method &rest args)
  "Create a handle file for PROJECT.
METHOD should be one of `bunel-method-names'. Optionally provide ARGS. "
  (shell-command (format "bunel %s %s %s"
                         project
                         (cdr (assoc method bunel-method-names))
                         (mapconcat 'identity args  " "))))


(defun bunel-refresh-client (&optional arg)
  "Refresh default idlegame unity project.
If ARG is non-nil, also enter playmode."
  (interactive)
  (bunel-create-handle-file bunel-default-unity-project 'bunel-refresh arg))

(defun bunel-refresh-all (&optional arg)
  "Refresh all Idlegames.
If ARG is non-nil, also enter playmode"
  (interactive)
  (mapcar (lambda (project) (bunel-create-handle-file project 'bunel-refresh arg)) bunel-idlegame-projects))

(defun bunel-save-and-refresh (&optional arg)
  "Save some buffers and refresh all Idlegames.
If ARG is non-nil, also enter playmode"
  (interactive)
  (save-some-buffers t)
  (bunel-refresh-all arg))
