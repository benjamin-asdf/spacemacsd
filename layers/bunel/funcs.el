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


(defun bunel/set-default-project ()
  "Set `bunel-default-unity-project'"
  (interactive)
  (setq bunel-default-unity-project (completing-read "Proj: " bunel-idlegame-projects)))

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
  (bunel--cmd "open-prefab" nil (completing-read "Prefab: " (bunel--prefabs))))

(defun bunel--prefabs ()
  "Get prefabs in assets folder using fd."
  (split-string
   (let ((default-directory idlegame-project-root))
     (shell-command-to-string
      "fd -I -tf -0 -E Fonts/ -e prefab . Assets/"))
   "\0"))


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

(defconst bunel/globals-asset-path "Assets/#/Scripts/Misc/Globals/Globals.asset")
(defconst bunel/globals-file (concat idlegame-project-root bunel/globals-asset-path))
(defvar bunel-set-globals-hist '())

(defun bunel-set-globals (item value)
  "Set ITEM in globals to VALUE."
  (interactive
   (let* ((item (completing-read "Field to set: " (bunel-read-yaml-file-fields bunel/globals-file) nil t nil 'bunel-set-globals-hist))
          (value (read-from-minibuffer (format "Set %s to: " item))))
     (list item value)))
  (bunel--set-value-in-yaml bunel/globals-file item value)
  (bunel--cmd "import-assets" nil bunel/globals-asset-path))

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

(defun benj-unity/file-usages-with-guid-at-point ()
  "Use `benj-unity/quick-file-usages' with the thing at point."
  (interactive)
  (benj-unity/quick-file-usages (thing-at-point 'evil-word)))

(defun benj-unity/quick-file-usages (&optional guid)
  "Search the project for the guid of the meta file you are visiting.
Or try to use the meta file of the file that you are visiting."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (buff-name "*quick-file-usages*")
         (guid (or (and (boundp 'guid) guid) (and buffer-file-name (benj-get-guid-with-meta buffer-file-name))))
         (usages (and guid (benj-unity/guid-file-usages guid))))
    (if usages
        (progn (pop-to-buffer buff-name)
               (erase-buffer)
               (insert (format "file usages for guid: %s\n" guid))
               (insert (mapconcat 'identity usages "\n")))
      (message "cannot get file usages"))))


(defun benj-unity/guid-file-usages (guid)
  "GUIDS file usages as list, non-zappy in large repos."
  (when-let ((default-directory (projectile-project-root)))
    (process-lines "git" "grep" "--files-with-matches" guid)))





;; implement timeout?

;; (let ((temp-sym (gensym)))
;;   (eval `
;;    (progn (defvar ,temp-sym "hello")
;;           (run-at-time 3 nil (// () (message ,temp-sym))))))


(defvar team-entr/when-laat-line-timeout 60)
(defmacro team-entr/when-last-line (file reg &rest body)
  "Use entr to wait until the last line of FILE matches REG.
Then execute BODY.
Use `team-entr/when-laat-line-timeout' for the timeout, default is 60. Binding `team-entr/when-laat-line-timeout' to nil will omitt a timout."
  (let ((proc-sym (gensym)))
    `(let* ((proc-name "entr")
            (proc
             (start-process-shell-command
              "entr"
              (format
               "*entr-%s*"
               (file-name-base ,file))
              (format
               "echo %1$s | entr -sp 'tac %1$s | head -n 1'"
               ,file))))
       (set-process-filter
        proc
        (// (proc string)
            (when (string-match-p ,reg string)
              (kill-process proc)
              ,@body)))
       ;; (when team-entr/when-laat-line-timeout
       ;;   (run-at-time team-entr/when-laat-line-timeout
       ;;                (// () (get-process ))))
       )))





(defvar bunel/unity-tests-last '())

(defun bunel/unity-unit-test-last ()
  (interactive)
  (unless bunel/unity-tests-last
    (user-error "no last test methods."))
  (bunel/unity-unit-test2 bunel/unity-tests-last))

(defun bunel/unity-unit-test-buffer ()
  "Runs all test cases defined in the current buffer.
see `omnisharp-unit-test-buffer'."
  (interactive)
  (omnisharp--cs-inspect-buffer #'bunel/unity-unit-test1))

(defun bunel/unity-unit-test1 (elements)
  "Start unit test with some omnisharp csharp data ELEMENTS."
  (let* ((test-methods (omnisharp--cs-filter-resursively
                        'omnisharp--cs-unit-test-method-p
                        elements))
         (test-method-framework (car (cdr (omnisharp--cs-unit-test-method-p (car test-methods)))))
         (test-method-names (mapcar (lambda (method)
                                      (car (omnisharp--cs-unit-test-method-p method)))
                                    test-methods))
         )
    (bunel/unity-unit-test2 test-method-names)))

(defun bunel/unity-test-at-point ()
  "Runs test case under point, if any. See `omnisharp-unit-test-at-point'"
  (interactive)
  (omnisharp--cs-element-stack-at-point
   (lambda (stack)
     (let* ((element-on-point (car (last stack)))
            (test-method (omnisharp--cs-unit-test-method-p element-on-point))
            (test-method-name (car test-method))
            (test-method-framework (car (cdr test-method))))
       (bunel/unity-unit-test2 (list test-method-name))))))

(defun bunel/unity-unit-test2 (method-names)
  (setq bunel/unity-tests-last
        (--mapcat
         (last (split-string it "\\."))
         method-names))
  (let ((out-file (make-temp-file "unity-test")))
    (apply #'bunel--cmd
           `("refresh-and"
             nil
             "run-tests"
             "-o" ,out-file
             ,@bunel/unity-tests-last))
    (message "Running %d unity tests..."
             (length bunel/unity-tests-last))
    (eval
     `(team-entr/when-last-line
       ,out-file
       "finished"
       (with-current-buffer-window
           "unity-tests"
           nil
           nil
         (erase-buffer)
         (insert-file-contents-literally
          ,out-file)
         (bunel/unity-test-mode))
       (delete-file ,out-file)))))



(define-derived-mode
  bunel/unity-test-mode
  fundamental-mode
  "bunel-unity-test"
  "Mode for team unity test commands"

  (font-lock-add-keywords
   'bunel/unity-test-mode
   '(("### PASSED" . 'unity-test-passed-face))))

(defface
  unity-test-passed-face
  '((((class color) (background dark))
     :foreground "green"))
  "")



;; (defun team-entr/watcher-window (out-file)
;;   "Open a window, start entr and cat the file."
;;   (with-current-buffer-window
;;       (format
;;        "*entr-%s*"
;;        (file-name-base out-file))
;;       nil
;;       nil
;;     (let ((proc
;;            (start-process-shell-command
;;             "entr"
;;             (current-buffer)
;;             (format
;;              "echo %1$s | entr -s 'cat %1$s'"
;;              out-file))))
;;       (set-process-filter
;;        proc
;;        (// (proc string)
;;            (internal-default-process-filter proc string)
;;            (when
;;                (string-match-p "FINISHED" string)
;;              (kill-process proc)))))))


;; (team-entr/when-last-line
;;  "out-file"
;;  "FINISHED"
;;  (message "finished bois"))
