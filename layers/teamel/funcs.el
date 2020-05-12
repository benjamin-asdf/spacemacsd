(defconst teamel-debug-buttons-file (concat idlegame-project-root "Assets/Editor/BestGlobals/DebugButtons.cs"))


(defun teamel-view-log ()
  "Usage: Click view complete raw in the job log, copy url into clipboard.
Expects some valid url in the unamed register"
  (interactive)
  (let ((buff-name "logs"))
    (pop-to-buffer buff-name)
    (erase-buffer)
    (follow-mode)
    (start-process "get-logs" buff-name "curl" (format "%s"(evil-get-register ?\")))))


(defun teamel-add-debug-button (&optional init)
  "Open debug buttons file and insert some stuff for a quick debug button.
If INIT is given, put it as start string for the method body."
  (interactive)
  (find-file teamel-debug-buttons-file)
  (goto-char (point-min))
  (re-search-forward "Ensures that the GoToGlobals button goes last")
  (forward-line 2)
  (insert
   "\n
    [Button(40)]
    [MenuItem(\"Best/besttest\")]
    static void besttest() {

    }
")
  (forward-line -2)
  (or (and init (insert init))
      (insert (make-string 2 ?\t)))
  (evil-insert-state))



(defun teamel-add-debug-button-with-region ()
  "Uses `teamel-add-debug-button' as subroutine.
Add debug button with region as init method body."
  (interactive)
  (teamel-add-debug-button (buffer-substring-no-properties (region-beginning) (region-end))))
