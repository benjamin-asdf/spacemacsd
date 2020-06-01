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
(defconst benj-unity/idlegame-tmp-path "/tmp/The Naughty Cult Ltd_/Clash of Streamers/")

(defconst bunel-method-names '((bunel-refresh . "refresh")
                               (bunel-open-scene . "open-scene")
                               (bunel-open-prefab . "open-prefab"))
  "List of commands to send to bunel")

(defconst bunel-window-name-lookup-file (concat idlegame-project-root "windows.best.files")
  "File to generate a lookup with window names. This is consumed by scripts that want to send open window commands.")

(defconst bunel-menu-enums-file (concat idlegame-project-root "Assets/#/Sources/Menu/MenuEnums.cs")
  "The Idlegame menu enums file. Used to generate menu names lookups.")

(defun bunel-create-handle-file (project method &rest args)
  "Create a handle file for PROJECT.
METHOD should be one of `bunel-method-names'. Optionally provide ARGS. "
  (start-process-shell-command "bunel"
                               "*bunel*"
                               project
                               (cdr (assoc method bunel-method-names))
                               (mapconcat 'identity args " ")))

(defun bunel-refresh-client (&optional arg)
  "Refresh default idlegame unity project.
If ARG is non-nil, also enter playmode."
  (interactive)
  (bunel-create-handle-file bunel-default-unity-project
                            'bunel-refresh arg))

(defun bunel-refresh-all (&optional arg)
  "Refresh all Idlegames.
If ARG is non-nil, also enter playmode"
  (interactive)
  (mapcar (lambda (project)
            (bunel-create-handle-file project 'bunel-refresh
                                      arg))
          bunel-idlegame-projects))

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
  (if (or (not (file-exists-p (concat bunel-unity-location
                                      (file-name-base bunel-bridge-file))))
          force)
      (unless (file-exists-p bunel-unity-location)
        (mkdir bunel-unity-location)
        (copy-file bunel-bridge-file bunel-unity-location))))


(defun bunel-open-unity-editor-log ()
  "Open unity log file in tail mode."
  (interactive)
  (find-file-literally unity-log-file)
  (goto-char (point-max))
  (follow-mode))



(defvar bunel-menu-lookups '()
  "This is a list of name and a plist.
Entries to the plist are
:file the file name to generate the lookup in,
:collect a symbol that should be bound to a function that collects the lookup")

(defun bunel-regenerate-open-menu-lookups ()
  "Ensure open menu lookups.
List for menus, overlays, windows to open."
  (interactive)
  (write-region (mapconcat 'identity
                           (bunel-get-windows-list)
                           "\n")
                nil
                bunel-window-name-lookup-file))


;; TODO abstract, figure out makros

(defun bunel-collect-menu-types ()
  "Search enum syntax in idlegame and collect overlay types."
  (split-string
   (with-output-to-string
     (with-temp-buffer
       (insert-file-contents-literally bunel-menu-enums-file)
       (search-forward "public enum MenuType {")
       (while (and (forward-line 1) (not (looking-at "}")) (not (= (point) (point-min))))
         (when (re-search-forward " +?\\(\\w+\\) = [0-9]+" (point-at-eol) t 1)
           (princ (concat (match-string 1) "\n"))))))))


(defun bunel-collect-overlay-types ()
  "Search enum syntax in idlegame and collect overlay types."
  (split-string
   (with-output-to-string
     (with-temp-buffer
       (insert-file-contents-literally bunel-menu-enums-file)
       (search-forward "public enum OverlayType {")
       (while (and (forward-line 1) (not (looking-at "}")) (not (= (point) (point-min))))
         (when (re-search-forward " +?\\(\\w+\\) = [0-9]+" (point-at-eol) t 1)
           (princ (concat (match-string 1) "\n"))))))))

;; (with-eval-after-load)


(defun bunel-open-overlay ()
  "Open overlay for default idlegame."
  (interactive)
  (shell-command (format "bunel %s open-overlay %s"
                         bunel-default-unity-project
                         (completing-read "Open overlay: " (bunel-collect-overlay-types)))))


(defun bunel-open-menu ()
  "Open menu for default idlegame."
  (interactive)
  (shell-command (format "bunel %s open-menu %s"
                         bunel-default-unity-project
                         (completing-read "Open menu: " (bunel-collect-menu-types)))))

(defun bunel--cmd (cmd proj &rest args)
  "Invoke bunel. PROJ default to `bunel-default-unity-project'.
CMD should be something."
  (interactive)
  (benj-start-proccess-flatten-args "bunel" "*bunel*" "bunel" (or proj bunel-default-unity-project) cmd args))



(defun bunel-open-prefab ()
  "Open prefab in idlegame unity"
  (interactive)
  (bunel--cmd "open-prefab" nil (completing-read "Prefab: " (bunel--prefabs)))
  ;; (bunel-create-handle-file bunel-default-unity-project 'bunel-open-prefab
  ;;                           (completing-read "Prefab: " (bunel--prefabs)
  ;;                                            ;; nil nil (when (string-equal (file-name-extension (buffer-file-name)) "prefab") (buffer-file-name))
  ;;                                            )
  ;;                           )
  )

;; TODO execute menu item
(defun bunel-open-debug-panel ()
  ""
  )

;; (defun bunel-prefab-completing-read ()
;;   "Select a prefab from assets folder."
;;   (completing-read ))

(defun bunel--prefabs ()
  "Get prefabs in assets folder using fd."
  (split-string
   (let ((default-directory idlegame-project-root))
     (shell-command-to-string
      "fd -I -tf -0 -E Fonts/ -e prefab . Assets/"))
   "\0"))


;; (defun bunel--collect-enum ()
;;   "Collect enum syntax, assumes format:
;; public enum Type {
;;    None = 100
;; }
;; ")


(defun bunel-get-windows-list ()
  "Search `bunel-menu-enums-file' for windows."
  (let ((res))
    (with-temp-buffer
      (insert-file-contents-literally bunel-menu-enums-file)
      (search-forward "public static class WindowType {"
                      nil t)
      (while (re-search-forward "public const string \\w+ = \"\\(\\w+\\)\";"
                                nil t)
        (setq res (-flatten (list res
                                  (match-string 1))))))
    res))


(defun bunel-get-menu-types ()
  "Generate a  list of strings with menu types. Search `bunel-menu-enums-file'.")



(defconst debug-method-file (concat idlegame-project-root "Assets/#/Sources/CheatTools/DebugMethods.cs"))
;; TODO args, and use global to do the collecting
(defun bunel-execute-debug-method (method)
  "Execute debug METHOD."
  (interactive
   (let ((method (completing-read "Debug method: " (bunel--collect-parameterless-funcs debug-method-file))))
     (list method)))
  (bunel--cmd "execute-debug-method" nil method))

;; (kill-new (completing-read "Debug method: " (bunel--collect-parameterless-funcs debug-method-file)))

;; should use global instead
(defun bunel--collect-parameterless-funcs (file)
  "Hack get some parameterless funcs from FILE."
  (split-string
   (with-output-to-string
     (with-temp-buffer
       (insert-file-contents-literally file)
       (while (re-search-forward "public void \\(\\w+\\)\\s+?()"  nil t)
         (princ (concat (match-string 1) "\n")))))))

(defun bunel-cheat-advance-time (days)
  "Cheat idlegame DAYS."
  (interactive"nDays to cheat: ")
  (bunel--cmd "cheat-advance-time" nil (number-to-string days)))


(defun bunel-execute-menu-item ()
  "Search menu item syntax, send command to execute."
  ;; (process-lines "rg" )
  ;; collect menu items with rg
  ;;
  ;;$ bunel IdleGame execute-menu-item "Tools/Generic/Toggle DebugOverlay &#c"
  (interactive)

  (let ((default-directory (concat idlegame-project-root "Assets/" "Editor/")))
    (process-lines "rg" "--color=never" "--no-heading" "-IN" "-o" "-e" "'\[MenuItem\(\"(.*)\"\)\]'" ))

  )

(defconst bunel-best-gloals-file (concat idlegame-project-root "Assets/#/Scripts/Misc/Globals/Globals.asset"))
(defvar bunel-set-globals-hist '())

(defun bunel-set-globals (item value)
  "Set ITEM in globals to VALUE."
  (interactive
   (let* ((item (completing-read "Field to set: " (bunel-read-yaml-file-fields bunel-best-gloals-file) nil t nil 'bunel-set-globals-hist))
          (value (read-from-minibuffer (format "Set %s to: " item))))
     (list item value)))
  (bunel--set-value-in-yaml bunel-best-gloals-file item value))

(defun bunel--set-value-in-yaml (file item value)
  "Try to set yaml syntax in FILE for ITEM to VALUE in current buffer."
  (with-temp-file file
    (insert-file-contents-literally file)
    (when (re-search-forward (format "\\(.*%s: \\)\\(.*\\)$" item))
      (replace-match (concat (match-string 1) (or (and (stringp value) value) (number-to-string value)))))))

(defun bunel-read-yaml-file-fields (file)
  ;; might want a list of key value pairs
  "Evaluate to a list of yaml fields in FILE"
  (split-string
   (with-output-to-string
     (with-temp-buffer
       (insert-file-contents-literally file)
       (while (re-search-forward "^\s+\\(\\w+\\): .+?$" nil t)
         (princ (concat (match-string 1) "\n")))))))

;; todo tests
;; (assert (member "skipTutorial" (bunel-read-yaml-file-fields bunel-best-gloals-file)))



;; (defmacro t-becomes-nil )


;; (defmacro t-becomes-nil (variable)
;;   `(if (eq ,variable t)
;;        (setq ,variable nil)))

;; (t-becomes-nil 10)


;; (t-becomes-nil foo)
;; ≡ (if (eq foo t) (setq foo nil))
