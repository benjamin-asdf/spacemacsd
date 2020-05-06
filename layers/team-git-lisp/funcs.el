
;;; Code:
(defun team-git-common-ancestor-as-kill ()
  "Copy common ancestor with develpo as kill."
  (interactive)
  (kill-new (team-git-common-ancestor)))

(defun team-git-common-ancestor (&rest revisions)
  "Get last common ancestor between REVISIONS.
Default to develop and HEAD, if REVISIONS has zero length,
if REVISIONS has the length 1, default to HEAD and arg"
  (let ((default-directory (projectile-project-root)))
    (string-trim (shell-command-to-string
                  (format "git merge-base %s %s"
                          (or (and revisions (car revisions)) "develop")
                          (or (and revisions (cadr revisions)) "HEAD"))))))




(defun benj-their-prefabs ()
  "Checkout theirs for all unmerged prefabs."
  (interactive)
  (let ((prefabs (benj-unmerged-prefabs)))
    (write-region (mapconcat 'identity prefabs "\n") nil "conflicted-prefabs")
    (async-shell-command (concat "git checkout --theirs " (mapconcat 'shell-quote-argument prefabs " ")))))


(defun benj-git-lfs-obj (rev &optional file)
  "Get git lfs obj for REV on FILE.
If FILE is ommitted try to get the current buffer file instead,
if REV is nil, use 'develop'."
  (string-trim
   (shell-command-to-string (concat "git-lfs-object" " " (or rev "develop") " " (shell-quote-argument (string-trim-left (or file buffer-file-name) (magit-toplevel)))))))

;;TODO
(defun benj-git-lfs-diff (rev1 rev2 &optional file)
  "View diff of git lfs object at FILE.
If FILE is ommitted, try use current buffer file.
IF REV1 is nill, use HEAD.
IF REV2 is null, use develop."

  )


(defun benj-write-unmermged-prefabs-to-file ()
  "Write current unmerged prefabs to file called unmerged-prefabs."
  (interactive)
  (write-region (mapconcat 'identity (benj-unmerged-prefabs) " ") nil "unmerged-prefabs"))


(defun team-curr-revision-as-kill (branch-name auto-insert)
  "Copy current git revision as kill.
If BRANCH-NAME is non nil, copy the branch name instead of commit sha.
If AUTO-INSERT is non nil, instantly insert at current buffer position."
  (let ((output (team-curr-revision branch-name))
        (message output))
    (when auto-insert (insert output))
    (kill-new output)))

(defun team-curr-revision (&optional branch-name)
  "Current git revision. If BRANCH-NAME is non nil, evaluate to the branch name instead of the commit sha."
  (team-remove-newline-end-of-string
   (team-projectile-dir-command-to-string
    (if branch-name "git branch --show-current" "git rev-parse HEAD"))))

  (defun team-diff-files (&optional rev)
    "Print the output of git diff --name-only to temp buffer.
If REV is non nil compare with REV instead of default develop."
    (interactive)
    (let ((other-rev (or rev "develop")))
      (with-output-to-temp-buffer (format "diff-%s..HEAD" other-rev)
        (print (team--git-diff-files other-rev))
        (print standard-output))))


(defun team-make-merge-request-commit ()
  "Run 'git commit --allow-empty -m '\do merge' in the current project dir."
  (interactive)
  (team-projectile-dir-command-to-string "git commit --allow-empty -m \"do /merge\""))

;; NOTE way more zappy than magit
(defun team-quick-commit (arg)
  "Run git commit in projectile root with ARG as commit message."
  (interactive "sCommit Message: ")
  (team-projectile-dir-command-to-string (format "git commit -m \"%s\"" arg)))

;; would need work (just scratch that)
;; TODO rename funcs to TEAM if you start using them

(defun team--git-diff-files (rev1 &optional rev2)
  "Get shell output for git diff files of REV1 agains REV2, if REV2 is ommitted, default to HEAD."
  (team-projectile-dir-command-to-string (format "git diff --name-only %s..%s" rev1 (or rev2 "HEAD"))))




  (defun benj-projectile-dir-command-to-string (command)
    "Run COMMAND with the current projectile project root as default dir.
Evaluate to the output string. See `shell-command-to-string'."
    (let ((default-directory (projectile-ensure-project (projectile-project-root))))
      (message (format "run command: %s in dir: %s" command default-directory))
      (shell-command-to-string command)))


  (defun benj--git-diff-files-list (rev1 &optional rev2)
    "Get git diff files of REV1 against REV2, if REV2 is ommitted, default to HEAD."
    (split-string (benj--git-diff-files rev1 rev2)))

  (defun benj--file-diff-string-match (file regex rev1 &optional rev2)
    "Evaluates to the matched STRING, in the output of git diff of FILE of REV1 agains REV2,
  If rev2 is omitted, default to HEAD."
    (let ((diff-output (benj-projectile-dir-command-to-string (format "git diff -p %s..%s -- %s" rev1 (or rev2 "HEAD") file))))
      (if (string-match regex diff-output)
          (match-string 0 diff-output)
        nil)))

  ;; (defun benj--get-diff-output-match-lines (regex rev1 &optional rev2)
  ;;   "Evaluate to a string containg the file names and lines matching REGEX
  ;; in the git diff output of REV1 against REV2,
  ;; if REV2 is ommitted it defaults to HEAD.
  ;; this also sets the return value of `match-string'."
  ;;   ;; TODO line number would be cool
  ;;   (let ((ret)
  ;;         (files (benj--git-diff-files-list rev1 rev2)))
  ;;     (dolist (file files)
  ;;       (benj--log-to-diff-output (format "%s %d%%" file (/ (cl-position file files) (* (length files) 1.0)) 100))
  ;;       (let ((match (benj--file-diff-string-match file regex rev1 rev2)))
  ;;         (when match (setq ret (concat ret "\n" (format "File:%s : %s" file match) ret)))))
  ;;     (or ret (format "No line differences found for %s against %s" regex rev1 rev2))))

  (defun benj--diff-output-match-lines (regex rev1 &optional rev2)
    "Open temp output buffer, show files and regex match of REGEX matching in the diff of REV1 against REV2
If REV2 is ommitted, default to HEAD."
    ;; (with-output-to-temp-buffer "*diff-match-lines*"
    ;;   (print (format "Git diff %s..%s, searching matching lines for %s ..." rev1 (or rev2 "HEAD") regex))
    ;;   (print (benj--get-diff-output-match-lines regex rev1 rev2))
    ;;   (print standard-output))
      (benj--log-to-diff-output (format "Git diff %s..%s, searching matching lines for %s ..." rev1 (or rev2 "HEAD") regex))
      (benj--log-to-diff-output (benj--get-diff-output-match-lines regex rev1 rev2)))


  (defun benj--log-to-diff-output (arg)
    "Log ARG as line to default diff output buffer in other window."
    (benj-log-output "*diff-match-lines*" arg))

  ;; TODO func that asks me to put stuff

  (defun benj-find-debugs-in-diff ()
    "Open temp buffer informing the user about matching added Debug.Logs
of HEAD agains develop."
    (interactive)
    (benj--diff-output-match-lines "^+.*Debug.Log(.*).*$" "develop"))


(defun benj-log-output (buff-name log)
  "Log string LOG in output buffer BUFF-NAME."
  (let ((curr-buff (current-buffer)))
    (unless (string-equal (buffer-name) buff-name)
      (switch-to-buffer-other-window buff-name))
    (insert (concat log "\n"))
    (switch-to-buffer-other-window curr-buff)))

(defun benj-remove-newline-end-of-string (string)
  "Remove newline characters at the end of STRING."
  (replace-regexp-in-string "\n\\'" "" string))

(defun benj--git-diff-files (rev1 &optional rev2)
  "Get shell output for git diff files of REV1 agains REV2, if REV2 is ommitted, default to HEAD."
  (benj-projectile-dir-command-to-string (format "git diff --name-only %s..%s" rev1 (or rev2 "HEAD"))))

