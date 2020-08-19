(defconst teamel-debug-buttons-file (concat idlegame-project-root "Assets/Editor/BestGlobals/DebugButtons.cs"))

(defun teamel/curl-yank ()
  "Usage: Click view complete raw in the job log, copy url into clipboard.
Expects some valid url in the unamed register"
  (interactive)
  (with-current-buffer-window
      "*curl-yank*"
      nil
      nil
      (teamel/curl-last-yank-this-buff)))

(defun teamel/curl-last-yank-this-buff ()
  (start-process
   "curl-website"
   (current-buffer)
   "curl"
   (teamel/last-yank)))

(defun teamel/digest-resharper-warnings ()
  "Invoke curl with the current yank.
Parse output for resharper warnings, this sets `teamel/these-resharper-warnings'."
  (interactive)
  (with-temp-buffer
    "*resharper-warnings*"
      nil
      nil
    (let ((exit-status
           (call-process
            "curl"
            nil
            (current-buffer)
            nil
            (teamel/last-yank))))
      (unless (eql exit-status 0)
        (error "err getting reshaper warnings: curl exited abnormally"))
      (teamel/parse-resharper-warnings))))

(defvar teamel/these-resharper-warnings '())

(defun teamel/parse-resharper-warnings ()
  "Evaluate to a list of the form (FILE . LINE)"
  (let ((res))
    (teamel/fix-backward-slashes)
    (goto-char (point-min))
    (while
        (and
         (skip-chars-forward "^<")
         (looking-at "<Issue")
         (re-search-forward ".+File=\"\\(.+?\\)\".*Line=\"\\([[0-9]+?\\)\"" (point-at-eol) t))
      (push
       (cons
        (concat
         (file-name-as-directory
          idlegame-project-root)
         (match-string-no-properties 1))
        (string-to-number (match-string-no-properties 2)))
       res))
    res))

(defun teamel/fix-backward-slashes ()
  (goto-char (point-min))
  (while (re-search-forward "\\\\" nil t)
    (replace-match "/")))

(defun teamel/last-yank ()
  (with-temp-buffer
    (yank)
    (buffer-string)))

(defun teamel/these-resharper-warning-files ()
  (-uniq (-map #'car teamel/these-resharper-warnings)))


(defun teamel/fix-menu-items-for-these-resharper-warnings ()
  "Depends on `teamel/these-resharper-warnings' being set,
add UsedImplicitly to all menu item syntax."
  (--map
   (team/with-file
    it
    (open-line 1)
    ;; FIXME there is a potential bug it the file already has
    ;; this names space
    (insert "using JetBrains.Annotations;")
    (team/while-reg
     "MenuItem"
     (->$ "[UsedImplicitly]")))
   (teamel/these-resharper-warning-files)))




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
    public void besttest() {

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

(defun teamel/idlegame-cog-purchase ()
  "Run cog idlegame."
  (interactive)
  (let ((default-directory "/home/benj/repos/codegen/"))
    (magit-run-git-with-editor "clean" "-fd")
    (magit-run-git-with-editor "pull" "origin" "refs/heads/master")
    (start-process "generate-purchase-data" "*generate-purchase-data*" "python3" "runner.py" "-s"))
  (pop-to-buffer "*generate-purchase-data*"))



(defun teamel/open-this-unity-proj ()
  "Open the untiy proj at PATH.
Use `projectile-locate-dominating-file' to get the unity proj root"
  (interactive)
  (when-let* ((proj (projectile-locate-dominating-file default-directory "Assets"))
              (name (file-name-base (directory-file-name proj))))
    (shell-command (format "unity-open %s %s" name proj))))


(defvar benj-flycheck/last-error-messages
  nil
  "Last error strings that `flycheck-pos-tip-error-messages' function was called with.")
(defun benj-flycheck/display-error-messages-advice (errors)
  "Advice after `flycheck-display-error-messages'. Set `benj-flycheck/last-error-messages' and also set register f with it."
  (evil-set-register
   ?f
   (setq benj-flycheck/last-error-messages (flycheck-help-echo-all-error-messages errors))))

(advice-add 'flycheck-pos-tip-error-messages :after #'benj-flycheck/display-error-messages-advice)


(defun teamel/yank-idlegame-sln ()
  (interactive)
  (kill-new (string-trim idlegame-sln-path)))


(defun teamel/yank-prefabs-for-rewrite ()
  "Try to search /IdleGame/prefab-analzyer-warnings.txt for paths and put them into /IdleGame/prefabs-for-rewrite.txt."
  (interactive)
  (let ((default-directory cos-dir))
    (team/with-file
     "IdleGame/prefabs-for-rewrite.txt"
     (insert
      (mapconcat
       #'identity
       (-uniq
        (team/with-file
         "prefab-analzyer-warnings.txt"
         (let ((res))
           (while (re-search-forward "Assets/Prefabs" nil t)
             (push (cadr (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                   res))
           res)))
       "\n")))))

(defun teamel/open-sources ()
  (interactive)
  (let ((default-directory (concat idlegame-assets-dir "#/Sources")))
    (helm-find-files-1 " ")))

(defun teamel/add-config-here ()
  "Take the current field at point. Search in the same file for some config syntax
and expand a snippet for a 'With...(this config)' method."
  (interactive)
  (let ((class-name)
        (field-name)
        (type))
    (save-excursion
      (line->0)
      (re-search-forward
       "public \\([^ ]+\\) \\(\\w+\\)"
       (line-end-position))
      (setq field-name (match-string-no-properties 2))
      (setq type (match-string-no-properties 1))
      (re-search-backward
       "public struct \\(\\w+\\) {"
       nil)
      (setq class-name (match-string-no-properties 1)))
    (benj-roslyn-tools//yasnippet-insert
     "autobconf"
     (format "public static %s With" class-name)
     `((class-name ,class-name)
      (field-name ,field-name)
      (type ,type)
      (field-name-capital ,(team/capitalize-first-letter field-name)))))
  (re-search-backward "public static class" nil)
  (forward-line 1)
  (open-line 1))





(defface teamel/flycheck-err-face
  '(
    (((class grayscale) (background light))
     :foreground "DimGray" :weight bold :slant italic)
    (((class grayscale) (background dark))
     :foreground "LightGray" :weight bold :slant italic)
    (((class color) (min-colors 88) (background light))
     :foreground "Firebrick")
    (((class color) (min-colors 88) (background dark))
     :foreground "chocolate1"
     :background "DarkSlateBlue")
    (((class color) (min-colors 16) (background light))
     :foreground "red")
    (((class color) (min-colors 16) (background dark))
     :foreground "red1")
    (((class color) (min-colors 8) (background light))
     :foreground "red")
    (((class color) (min-colors 8) (background dark))
     :foreground "yellow")
    (t :weight bold :slant italic))
  "Face to display team flycheck errs."
  :group 'teamel/faces)


(defun teamel/flycheck-momentary-string-display-function (errors)
  "Not documented."
  (momentary-string-display
   (with-temp-buffer
     (insert (flycheck-help-echo-all-error-messages errors))
     (put-text-property
      (point-min) (point-max)
      'face 'teamel/flycheck-err-face
      )
     (buffer-string))
   (save-excursion
     (forward-line -10)
     (re-search-forward
      "^$" (save-excursion
             (forward-line 20)
             (point-at-eol))
      t
      1)
     (point-at-bol))))


(setq flycheck-display-errors-function
      #'teamel/flycheck-momentary-string-display-function)
