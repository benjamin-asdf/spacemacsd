(defconst bunel-handle-path "/tmp/bunel-handles"
  "Directory path where the bunel bridge searches for handle files")
(defconst bunel-idlegame-projects '("IdleGame" "IdleGameSymbolicLink" "IdleGameSymbolicLink-Extra")
  "List of possible projects to create handles for")
(defconst bunel-default-unity-project "IdleGame"
  "The default unity project to operate on")
(defconst bunel-bridge-file "/home/benj/repos/bunel/BenjBridge.cs"
  "Path of a unity editor script that handles bunel requests")
(defconst bunel-unity-location "/home/benj/idlegame/IdleGame/Assets/Editor/benj-private/"
  "Place to put the unity editor script")

(defconst unity-log-file "~/.config/unity3d/Editor.log"
  "Location of the unity log file")

(defconst bunel-method-names
  '((bunel-refresh . "refresh")
    (bunel-open-scene . "open-scene"))
  "List of commands to send to bunel")

(defconst bunel-window-name-lookup-file (concat idlegame-project-root "windows.files")
  "File to generate a lookup with window names. This is consumed by scripts that want to send open window commands.")

(defconst bunel-menu-enums-file (concat idlegame-project-root "Assets/#/Sources/Menu/MenuEnums.cs")
  "The Idlegame menu enums file. Used to generate menu names lookups.")


(defun bunel-create-handle-file (project method &rest args)
  "Create a handle file for PROJECT.
METHOD should be one of `bunel-method-names'. Optionally provide ARGS. "
  (start-process-shell-command "bunel" "*bunel*" project (cdr (assoc method bunel-method-names)) (mapconcat 'identity args  " ")))

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


(defun bunel-ensure-bridge-file (&optional force)
  "Ensure bridge file is in the idlegame project.
If FORCE is non nil, override any existing file."
  (interactive)
  (if (or (not (file-exists-p (concat bunel-unity-location (file-name-base bunel-bridge-file)))) force)
    (unless (file-exists-p bunel-unity-location)
      (mkdir bunel-unity-location)
      (copy-file bunel-bridge-file bunel-unity-location))))


(defun bunel-open-unity-editor-log ()
  "Open unity log file in tail mode."
  (interactive)
  (find-file-literally unity-log-file)
  (goto-char (point-max))
  (follow-mode))

(defun bunel-regenerate-open-menu-lookups ()
  "Ensure open menu lookups.
List for menus, overlays, windows to open."
  (interactive)
  (write-region (mapconcat 'identity (bunel-get-windows-list) "\n") nil bunel-window-name-lookup-file))



(defun bunel-get-windows-list ()
  "Search `bunel-menu-enums-file' for windows."
  (let ((res))
    (with-temp-buffer
      (insert-file-contents-literally bunel-menu-enums-file)
      (search-forward "public static class WindowType {" nil t)
      (while (re-search-forward "public const string \\w+ = \"\\(\\w+\\)\";" nil t)
        (setq res (-flatten (list res (match-string 1)))))
      )
    res))


(defun bunel-get-menu-types ()
  "Generate a  list of strings with menu types. Search `bunel-menu-enums-file'."


  )



;; (defmacro )



;; (defmacro t-becomes-nil )


;; (defmacro t-becomes-nil (variable)
;;   `(if (eq ,variable t)
;;        (setq ,variable nil)))

;; (t-becomes-nil 10)


;; (t-becomes-nil foo)
;; ≡ (if (eq foo t) (setq foo nil))
