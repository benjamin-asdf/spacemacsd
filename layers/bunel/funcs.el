(defconst bunel-handle-path "/tmp/bunel-handles")
(defconst bunel-idlegame-projects '("IdleGame" "IdleGameSymbolicLink" "IdleGameSymbolicLink-Extra"))
(defconst bunel-default-unity-project "IdleGame")
(defconst bunel-bridge-file "/home/benj/repos/bunel/BenjBridge.cs")
(defconst bunel-unity-location "/home/benj/idlegame/IdleGame/Assets/Editor/benj-private/")


(defconst bunel-method-names
  '((bunel-refresh . "refresh")
    (bunel-open-scene . "open-scene")))

(defun bunel-create-handle-file (project method &rest args)
  "Create a handle file for PROJECT.
METHOD should be one of `bunel-method-names'. Optionally provide ARGS. "
  ;; (shell-command (format "bunel %s %s %s" #TODO TEMP small one time hack
  (shell-command (format "/home/benj/.local/bin/bunel/bunel %s %s %s"
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

;; (defun bunel-open-scene (project scene)
;;   "Open SCENE in PROJECT."
;;   (interactive)
;;   )


(defun bunel-ensure-bridge-file (&optional force)
  "Ensure bridge file is in the idlegame project.
If FORCE is non nil, override any existing file."
  (interactive)
  (if (or (not (file-exists-p (concat bunel-unity-location (file-name-base bunel-bridge-file)))) force)
    (unless (file-exists-p bunel-unity-location)
      (mkdir bunel-unity-location)
      (copy-file bunel-bridge-file bunel-unity-location))))



;; (add-hook 'git-commit-post-finish-hook '(lambda () (message "This is the commit-post-finish-hook!")))




(defun bunel-open-unity-editor-log ()
  "Open unity editor log other window.
;; TODO linux"
  (interactive)
  (find-file "c:/Users/G4G/AppData/Local/Unity/Editor/Editor.log"))
