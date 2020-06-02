
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


(defun team-git-clear-merge-temp-files ()
  "Use fd to find and delete temp files."
  (interactive)
  (let* ((default-directory (magit-toplevel))
         (do-stash (not (null (magit-staged-files)))))
    ;; (files (process-lines "fd" "-I" "--full-path" "'\.orig\(.meta\)?$'"  "-x" "rm"))
    ;; todo find files recursive, git rm + git commit
    (when do-stash
      (benj-run-git-sync "stash"))
    (shell-command "fd -I --full-path '\.orig\(.meta\)?$' -x git rm")
    (when (magit-staged-files)
      (benj-run-git-sync "commit" "-m" "Delete orig files\n\nThese should not be commited. Be more careful what you commit, bro"))
    (when do-stash
      (benj-run-git "stash" "pop"))))


(defun benj-their-prefabs (&optional arg)
  "Checkout theirs for all unmerged prefabs. If ARG is non nil, also write a file for prefabs to rewrite."
  (interactive)
  (let ((prefabs (benj-unmerged-prefabs)))

    (write-region (mapconcat 'identity prefabs "\n") nil "conflicted-prefabs")
    (benj-checkout-stage "--theirs" prefabs)))

(defun benj-our-prefabs ()
  "See `benj-checkout-stage'"
  (interactive)
  (benj-checkout-stage "--ours" (benj-unmerged-prefabs)))

(defun benj-merge-prefabs ()
  "Run mergetool with unmerged prefabs."
  (interactive)
  (benj-run-git "mergetool" "--no-prompt" "--" (mapcar 'shell-quote-argument (benj-unmerged-prefabs))))

(defun benj-write-unmerged-prefabs-to-prefabs-for-rewrite ()
  "Write the currently unmerged prefabs into a file called Idlegame/prefabs-for-rewrite.txt."
  (interactive)
  (let ((default-directory idlegame-project-root))
    (write-region (mapconcat (lambda (p) (string-trim-left p "IdleGame/")) (benj-unmerged-prefabs) "\n") nil "prefabs-for-rewrite.txt")))


(defun benj-checkout-stage (arg files)
  "Checkout FILES, which should be list of paths or a single string, ARG can be either
--ours, --theirs,
TODO: --merge."
  ;; See `magit-checkout-stage', but I wanted to not invoke it foreach file
  (let ((file-arg (or (and (listp files) (mapcar 'shell-quote-argument files)) files)))
    (benj-run-git-sync "checkout" arg "--" file-arg)
    (benj-run-git "add" "-u" "--" file-arg)))


(defun benj/magit-resolve-all-files ()
  "Ask user for each unmerged file which stage to checkout see `magit-discard-files--resolve'."
  (interactive)
  (while (magit-discard-files--resolve (magit-unmerged-files))))

(defun benj-run-git (&rest args)
 "Run git in *benj-git* buffer with current `magit-toplevel' as default directory."
 ;; TODO put the command in the buffer
 (let* ((default-directory (magit-toplevel))
        (buff-name "*benj-git*")
        (proc (benj-start-proccess-flatten-args "benj-git" buff-name "git" args)))
   (pop-to-buffer buff-name)
   ;; (insert (format "git.. %s" (mapconcat 'identity args " ")))
   proc))



;; TODO just use `call-process'
(defun benj-run-git-sync (&rest args)
  "Use `benj-run-git' and wait for process to finish. Returns the process"
  (let ((proc (benj-run-git args)))
    (while (accept-process-output proc))
    proc))

(defun benj-add-deletion-to-file (file)
  "Run git rm on FILE"
  (benj-run-git "rm" file))

;; todo
(defun benj-find-git-lfs-obj ()
  "Find git lfs object file, for REV and FILE, default to develop and current buffer file"
  (interactive)
  (find-file (benj-git-lfs-obj nil)))

(defun benj-find-git-lfs-obj-develop ()
    (interactive)
    (find-file (benj-git-lfs-obj "develop")))

(defun benj-git-lfs-obj (rev &optional file)
  "Get git lfs obj for REV on FILE.
If FILE is ommitted try to get the current buffer file instead,
if REV is nil, use 'develop'."
  (string-trim
   (shell-command-to-string (concat "git-lfs-object" " " (or rev "HEAD") " " (shell-quote-argument (string-trim-left (or file buffer-file-name) (magit-toplevel)))))))

;;TODO
(defun benj-git-lfs-diff (rev1 rev2 &optional file)
  "View diff of git lfs object at FILE.
If FILE is ommitted, try use current buffer file.
IF REV1 is nill, use HEAD.
IF REV2 is null, use develop."
  (ediff (benj-git-lfs-obj rev1 file) (benj-git-lfs-obj rev2 file)))


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
  (string-trim (shell-command-to-string (or (and branch-name "git branch --show-current") "git rev-parse HEAD"))))

(defun benj-git/diff-files-only (rev-or-range &optional other-rev)
  "Pop to a buffer to diff file names only against REV-OR-RANGE and optionaly OTHER-REV."
  (interactive
   (list (magit-read-branch-prefer-other "Ref or range to diff file names: ")))
  (switch-to-buffer-other-window (format "*diff-files-against-%s" rev-or-range))
  (insert
   (with-output-to-string
     (princ (mapconcat 'identity (magit-changed-files rev-or-range (and (boundp 'other-rev) other-rev)) "\n")))))

(defun team-make-merge-request-commit ()
  "Run 'git commit --allow-empty -m '\do merge' in the current project dir."
  (interactive)
  (benj-run-git "commit" "--allow-empty" "-m" "do /merge"))

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


(defun benj-git/resolve-conflicts-interactive ()
  "Interactively resolve all merge conflicts, ask individually."
  (interactive)
  (--map
   (let* ((arg (magit-read-char-case
                   (format "%s is %s \ncheckout:\n" (car it) (cadr it))
                   t
                 (?o "[o]ur stage"   "--ours")
                 (?t "[t]heir stage" "--theirs")))
          (ours (string-equal arg "--ours")))
     (pcase (cadr it)
       ("AU"
        (if ours
            (benj-checkout-stage arg (car it))
          ;; added by us, they don't have a version, so rm
          (benj-run-git-sync "rm" "--" (car it))))
       ("UA"
        ;; added by them,
        ;; actually not sure how you have that, not by deleting on our side, that would be DU.
        ;; I think it only happens with renames maybe
        (if ours
            (benj-run-git-sync "rm" "--" (car it))
            (benj-checkout-stage arg (car it))))
       ("DD" (benj-run-git-sync "rm" "--" (car it)))
       ("DU"
        (if ours
            (benj-run-git-sync "rm" "--" (car it))
          (benj-checkout-stage arg (car it))))
       ("UD"
        (if ours
            (benj-checkout-stage arg (car it))
          (benj-run-git-sync "rm" "--" (car it))))
       ("UU"  (benj-checkout-stage arg (car it)))))
   (benj-git/unmerged-status)))

(defun benj-git/unmerged-status ()
  "Get unmerged files in the format defined by `benj-git/git-status-files'."
  (let ((unmerged (magit-unmerged-files)))
    (--filter (member (car it) unmerged) (benj-git/git-status-files))))

(defun benj-git/git-status-files ()
  "Return a list of unemerged file in the format (FILE STATUS).
For documentation on the status codes see git-status man."
  (let ((default-directory (magit-toplevel)))
    (--map
     (list (substring it 3 (length it)) (substring it 0 2))
     (process-lines "git" "status" "--porcelain"))))


(defvar benj-git/last-visited-unmerged-cs-file "")
(defun benj-git/goto-next-unmerged-cs-file ()
  "Goto to the next unmerged cs file."
  (interactive)
  (let* ((default-directory (magit-toplevel))
         (files (--filter (string-match-p "\.cs$" it) (magit-unmerged-files))))
    (if files
        (progn
          (message "There are %d unmerged cs files still." (length files))
          (find-file
          (setq benj-git/last-visited-unmerged-cs-file
                (nth (% (+ (or (cl-position benj-git/last-visited-unmerged-cs-file files :test 'equal) -1) 1) (length files)) files))))
      (message "No more unmerged cs files!"))))

(defun benj-git/send-input-to-process ()
  "Ghetto send some string with newline to git process."
  (send-string (get-process "benj-git") (format "%s\n" (read-from-minibuffer "Send string: "))))

(defun benj-git/fetch-and-merge ()
  "First fetch dev, then merge dev."
  ;; TODO something with direnv where we check the primary fetch branch
  (interactive)
  (benj-run-git-sync "fetch" "origin" "develop:develop")
  (benj-run-git "merge" "develop" "--no-edit"))
