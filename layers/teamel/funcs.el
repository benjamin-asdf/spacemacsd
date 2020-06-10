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

;; todo
;;(defun teamel-delete-scriptass ())

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



(defun teamel-open-latest-video ()
  "Open latest file in ~/Videos with vlc."
  (interactive)
  ;; TODO use prefered app instead of vlc
  (start-process "latest-vid" "latest-vid" "vlc" (latest-file
                                                  "~/Videos")))



;; (helm :sources helm-benj-git-diff-lines-source)

;; (defun benj-git-diff-lines ()
;;   "Git diff output as lines."
;;   (let ((default-directory (magit-toplevel)))
;;     (process-lines "git" "diff" "-p" )))
;; (with-eval-after-load 'helm
;;   (defvar helm-benj-git-diff-lines-source
;;     (helm-build-sync-source "GitDiff" :candidates (benj-git-diff-lines))))


;; works
;;(helm :sources (helm-build-sync-source "name" :candidates '(( "best" . "bestreal" ) ("hehe" . "hehereal"))))


(defun teamel/regenerate-purchase-idlegame ()
  "Run cog idlegame."
  (interactive)
  (let ((default-directory (concat cos-dir "/modules" "/codegen")))
    (shell-command "git pull origin master")
    (start-process "generate-purchase-data" "*generate-purchase-data*" "python3" "benj-runner.py" "-s"))
  (pop-to-buffer "*generate-purchase-data*"))



(defun teamel/open-this-untiy-proj ()
  "Open the untiy proj at PATH.
Use `projectile-locate-dominating-file' to get the unity proj root"
  (interactive)
  (when-let* ((proj (projectile-locate-dominating-file default-directory "Assets"))
              (name (file-name-nondirectory proj)))
    (start-process (format "unity-open-%s" name) (format "*unity-s%*" name) "unity-open" name proj)))
