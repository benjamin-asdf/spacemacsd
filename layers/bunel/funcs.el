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



(defvar bunel/last-cmd '())
(defvar bunel/last-debug-method-cmd '()
  "Flat list of cmd and args")
(defun bunel--cmd (cmd proj &rest args)
  "Invoke bunel. PROJ default to `bunel-default-unity-project'.
CMD should be something."
  (interactive)
  (setq bunel/last-cmd `(,cmd ,@args))
  (when (string-equal "execute-debug-method" cmd)
    (setq bunel/last-debug-method-cmd bunel/last-cmd))
  (team/start-proc "bunel" "*bunel*" "bunel" (or proj bunel-default-unity-project) cmd args))


(defmacro bunel/chain (cmd &rest more)
  "Create a cmd chain. Forms
Project will be `bunel-default-unity-project'."
  (if (null cmd)
      nil
    `(progn
       (apply
        #'bunel--cmd
        (append
         (list ,cmd nil)
         ,(when (listp (car more)) `,@(pop more))))
       (bunel/chain ,(car more) ,@(cdr more)))))




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

(defun bunel/collect-menu-types ()
  "Search enum syntax in idlegame and collect overlay types."
  (split-string
   (with-output-to-string
     (with-temp-buffer
       (insert-file-contents-literally bunel-menu-enums-file)
       (search-forward "public enum MenuType {")
       (while (and (forward-line 1) (not (looking-at "}")) (not (= (point) (point-min))))
         (when (re-search-forward " +?\\(\\w+\\) = [0-9]+" (point-at-eol) t 1)
           (princ (concat (match-string 1) "\n"))))))))

(defun bunel/collect-overlay-types ()
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
                         (completing-read "Open overlay: " (bunel/collect-overlay-types)))))


(defun bunel-open-menu ()
  "Open menu for default idlegame."
  (interactive)
  (shell-command (format "bunel %s open-menu %s"
                         bunel-default-unity-project
                         (completing-read "Open menu: " (bunel/collect-menu-types)))))




(defun bunel/rerun-last ()
  (interactive)
  (bunel/rerun--last bunel/last-cmd))

(defun bunel/rerun-debug-method ()
  (interactive)
  (bunel/rerun--last bunel/last-debug-method-cmd))

(defun bunel/rerun--last (cmd)
  (team/a-if
   cmd
   (progn (bunel--cmd
           (car it)
           nil
           (cdr it))
          (message "rerun %s" (car it)))
   (user-error "No last unity cmd.")))


(defun bunel/refresh-and-play (arg)
  "Refresh and play, with prefix ARG do not enter play mode."
  (interactive"P")
  (save-some-buffers)
  (bunel--cmd
   "refresh-and"
   nil
   (unless arg "playmode")))

(defun bunel/loading-scene-and-play ()
  (interactive)
  (bunel--cmd
   "refresh-and"
   nil
   "open-scene"
   "Assets/Scenes/LoadingScene.unity"))

(defun team/import-asset (f)
  (interactive"f")
  (bunel--cmd
   "import-assets"
   nil
   f))

(defun team-unity/open-prefab-at-point ()
  (interactive)
  (team/a-when-reg-this-line
   "\\(.*\.prefab\\)" 0
   (bunel-open-prefab
    (if (string-prefix-p "Assets/" it)
        it
      (concat "Assets/" it)))))

(defun bunel-open-prefab (&optional prefab)
  "Open prefab in idlegame unity"
  (interactive)
  (bunel--cmd
   "open-prefab"
   nil
   (or
    prefab
    (completing-read "Prefab: " (bunel--prefabs)))))

(defun bunel/scene-view ()
  (interactive)
  (bunel--cmd
   "scene-view"
   nil
   (or
    prefab
    (completing-read "Prefab: " (bunel--prefabs)))))

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
   (list (completing-read "Debug method: " (bunel--collect-parameterless-funcs debug-method-file))))
  (bunel--cmd "execute-debug-method" nil method))


;; should use global instead
(defun bunel--collect-parameterless-funcs (file)
  "Hack get some parameterless funcs from FILE."
  (team/collect-reg
   file
   "void \\(\\w+\\)\\s+?()"
   1))

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


(defun bunel/set-debug-lore ()
  (interactive)
  (bunel-set-globals "disableLore" "0")
  (bunel-set-globals "loreAfterTutorial" "1"))


;; generic unity utils

(defun unity/trim-to-asset-path (path)
  (team/re-replace-in-string path
                             "^.*\\(Assets/.*$\\)"
                             "\\1"))

(defun team-unity/file-or-meta (file)
  (if (string-equal (file-name-extension file) "meta")
      file
    (concat file ".meta")))

(defun benj-all-guids-at-path (dir)
  "All unity guids of metas in DIR"
  (mapcar 'benj-get-guid
          (benj-directory-files dir ".*meta")))


(defun team-unity/file-guid (file)
  "Get guid for FILE. If FILE is not a meta file, try to use the corresponding meta file."
  (car
   (team/collect-reg
    (team-unity/file-or-meta file)
    "guid: \\(\\w+\\)" 1)))




;; guid searches

(defun team-unity/rg-guid-search (&optional file)
  "Use rg to search the project for the guid of the visiting file."
  (interactive)
  (team/a-if
   (or file (buffer-file-name))
   (team-unity/rg--guid-search
    (team-unity/file-guid
     it))
   (user-error "Not visiting a file.")))

(defun team-unity/rg--guid-search (guid)
  "Use rg to search the project GUID. Put output in a temp buffer."
  (require 'idlegame-definitions)
  (team/with-default-dir
   idlegame-assets-dir
   (with-current-buffer-window
       "*unity-guid-search*"
       nil
       nil
     (erase-buffer)
     (team/insert-line (format "file usages for guid: %s ...\n" guid))
     (start-process
      "*unity-guid-search*"
      (current-buffer)
      "rg"
      "-IlN"
      (format "guid: %s" guid)))))

(defun team-unity/rg-guid--search-args (guid)
  `(
    "rg"
    "rg"
    "-IlN"
    ,(format "guid: %s" guid)))


(defun team-unity/rg-guid-search-at-point ()
  (interactive)
  (team-unity/rg--guid-search
   (thing-at-point 'evil-word)))

(defun team-unity/rg-guid-search-ask-file (file)
  (interactive"f")
  (team-unity/rg-guid-search file))



(defun team-unity/set-prefab-root-active (file active)
  (team/with-file
   file
   (re-search-forward "\\(^  m_IsActive: \\)[10]$")
   (replace-match (format "\\1%s" (if active "1" "0")))))



(defun benj-msg-time-string ()
  "Put curr time string in the echo area."
  (interactive)
  (message (current-time-string)))






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



(defconst bunel/test-buff-name "*unity-tests*")
(defvar-local bunel/test-out-file '())
(defun bunel/unity-unit-test2 (method-names)
  (setq bunel/unity-tests-last method-names)
  (let ((out-file (make-temp-file "unity-test-out")))
    (with-current-buffer
        (get-buffer-create bunel/test-buff-name)
      (setq bunel/test-out-file out-file))
    (apply #'bunel--cmd
           `("refresh-and"
             nil
             "run-tests"
             "-o" ,out-file
             ,@bunel/unity-tests-last))
    (message "Running %d unity tests..."
             (length bunel/unity-tests-last))
    (team-entr/file-changed-cb
     out-file
     (lambda ()
       (with-current-buffer
           bunel/test-buff-name
           (erase-buffer)
           (insert-file-contents-literally
            bunel/test-out-file)
           (team/with-default-dir
            "/tmp" (delete-file bunel/test-out-file))
           (pop-to-buffer (current-buffer))
           (bunel/unity-test-mode)
           (font-lock-fontify-buffer))))))

(define-derived-mode
  bunel/unity-test-mode
  fundamental-mode
  "bunel-unity-test"
  "Mode for team unity test commands"

  (font-lock-add-keywords
   'bunel/unity-test-mode
   '(("### PASSED" . 'bunel/test-passed-face)
     ("### ALL TESTS PASSED ###" . 'bunel/test-passed-bottom)
     ;; ("\[Test\]" . '(:foreground "Yellow"))
   ))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(defface
  bunel/test-passed-face
  '((t . (:foreground  "LawnGreen") ))
  "")

(defface
  bunel/test-passed-bottom
  '((t . (:foreground  "SpringGreen" :underline "white")))
  "")



(defun team-entr/file-changed-cb (file cb)
  "Run cb the first time FILE changes."
  (with-current-buffer
      (get-buffer-create
       (format
        "*entr-%s*"
        (file-name-base file)))
    (set (make-local-variable 'entr-cb) cb)
    (set-process-sentinel
    (start-process-shell-command
     "entr"
     (current-buffer)
     (format
      "echo %1$s | entr -sp 'kill $PPID'"
      file))
    (lambda (p e)
      (pcase (process-exit-status p)
        (1 (error "Entr did no start, no such file"))
        (_ (with-current-buffer
               (process-buffer p)
             (funcall entr-cb))) ; 143
        )))))




(defconst my/conflicted-prefabs-file "/tmp/conflicted-prefabs")
(defun cos/write-conflicted-prefabs-to-file ()
  "Collect conflicted prefabs as lines in `my/conflicted-prefabs-file'."
  (require 'benj-magit)
  (interactive)
  (write-region
   (mk-lines (benj-unmerged-prefabs))
   nil
   my/conflicted-prefabs-file))

(defun cos/check-conflicted ()
  "Run prefab checker on prefabs in `my/conflicted-prefabs-file'."
  (interactive)
  (unless (file-exists-p my/conflicted-prefabs-file)
    (user-error "Do not have cached conflicted prefabs"))
  (cos/prefab-integrity-check
   (team/file-lines "/tmp/conflicted-prefabs")))

(defun cos/prefab-integrity-check (&rest files)
  "Run checker with FILES."
  (interactive)
    (team/with-default-dir
  cos-dir
  (with-current-buffer
      (get-buffer-create "*prefab-check*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (team/proc-with-cb
    (apply
     #'start-process
     `("prefab-check-dotnet"
       ,(current-buffer)
       "dotnet"
       "/home/benj/repos/csharp/prefab-checker/bin/Debug/netcoreapp3.1/publish/prefab-checker.dll"
       ,@(-flatten files)))
    t
    (pop-to-buffer
     (current-buffer))
    (->gg)))))

(defun cos/do-prefab-integrity-check (&optional file)
  "Run merge checker on a single FILE."
  (interactive
   (list (concat "IdleGame/" (completing-read "Prefab: " (bunel--prefabs)))))
  (unless (string-match-p ".*prefab$" file) )
  (cos/prefab-integrity-check file))




(add-to-load-path "~/.spacemacs.d/layers/bunel/")

(defun team-unity/lazy-fix-load-group (file)
  (interactive"f")
  (require 'unity-addressables)
  (fix-load-group file))

(defun team-unity/lazy-add-labels (file-or-meta -labels)
  (require 'unity-labels)
  (team-unity/add-labels file-or-meta -labels))

(defun team-unity/lazy-do-add-label ()
  (require 'unity-labels)
  (interactive)
  (command-execute #'cos/add-prefab-label-and-add-for-rewrite nil nil))
