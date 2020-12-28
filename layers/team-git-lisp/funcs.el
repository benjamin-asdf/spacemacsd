;; -*- lexical-binding: t; -*-
;;(require 'magit)

;;; Code:

(defvar-local benj-git/after-magit-op nil)
(defun benj-git/after--magit (op &optional run-if-non-active)
  "Call OP after magit process finished with a single arg, the exit status.
If RUN-IF-NON-ACTIVE is non nil, call OP even if there is no magit process with
arg 222."
  (and
   run-if-non-active
   (or (not magit-this-process)
       (memq (process-status magit-this-process) '(exit signal)))
   (funcall op 222))
  (when
   magit-this-process
   (with-current-buffer (process-buffer magit-this-process)
     (setq-local benj-git/after-magit-op op))
   (set-process-sentinel
    magit-this-process
    (lambda (process event)
      (with-current-buffer (process-buffer process)
        (when (memq (process-status process) '(exit signal))
          (if (> (process-exit-status process) 0)
              (magit-process-sentinel process event)
            (process-put process 'inhibit-refresh t)
            (magit-process-sentinel process event)
            (funcall benj-git/after-magit-op (process-exit-status process)))))))))

(defmacro benj-git/after-magit-success (&rest body)
  (declare (debug body))
  `(benj-git/after--magit
    (lambda (e)
      (when (= 0 e)
        ,@body))))

(defmacro team/magit-pipe (&optional form &rest forms)
  "Assume each form starts a new magit process.
Run FORM, then each of FORMS after the previous git process exited with status 0.
When there is no magit process or it returns non 0, stop.
If you want to eval multiple forms for side effects. Put some progn that also sets the next git proccess."
  (if (null form)
      nil
    `(progn ,form
            (benj-git/after-magit-success
             (team/magit-pipe ,(car forms) ,@(cdr forms))))))



(defun team/git-ls-tree-files (rev)
  (team/start-buffer-process
   "*ls-tree*"
   "git"
   "ls-tree"
   "-r"
   "--name-only"
   rev))




(defun team/magit-is-ancestor (&optional a b)
  "Use magit to read 2 revs, print message if A is ancestor of B."
  (interactive
   (list
    (magit-read-branch-or-commit "Ancestor")
    (magit-read-branch-or-commit "Descendant")))
  (message
   "%s is %sancestor of %s"
   a
   (if (magit-rev-ancestor-p a b)
       ""
     "not ")
   b))

(defun team/magit-common-ancestor ()
  (interactive)
  (kill-new (magit-git-string-ng "merge-base" "HEAD" (magit-read-branch-prefer-other "Get ancestor with"))))


;; (defun team/magit-common-ancestor-many ()
;;   (interactive)
;;   (catch 'user-error "Nothing selected"
;;          (let ((next))
;;            (while (not (equal next "Nothing selected"))
;;              (magit-read-branch-or-commit "Next rev for ancestors"))))
;;   )

(defun team//magit-common-ancestor (&rest revs)
  (magit-git-string-ng "merge-base" revs))


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

(defun benj-all-theirs ()
  (interactive)
  (benj-checkout-stage "--theirs" (magit-unmerged-files)))


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

;; TODO refactor using `magit-call-git' etc.
(defun benj-run-git (&rest args)
 "Run git in *benj-git* buffer with current `magit-toplevel' as default directory."
 ;; TODO put the command in the buffer
 (let* ((default-directory (magit-toplevel))
        (buff-name "*benj-git*")
        (proc (benj-start-proccess-flatten-args "benj-git" buff-name "git" args)))
   (pop-to-buffer buff-name)
   ;; (insert (format "git.. %s" (mapconcat 'identity args " ")))
   proc))

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

(defun benj-git/idlegame-rev-as-kill (branch-name auto-insert-mode)
  "See `team-curr-revision-as-kill', use `cos-dir' as default dir."
  (let ((default-directory cos-dir))
    (team-curr-revision-as-kill branch-name auto-insert-mode)))

(defun team-curr-revision (&optional branch-name)
  "Current git revision. If BRANCH-NAME is non nil, evaluate to the branch name instead of the commit sha."
  (require 'magit)
  (funcall (if branch-name #'magit-rev-name #'magit-rev-hash) "HEAD"))

(defun benj-git/diff-files-only (&rest refs)
  "Pop to a buffer to diff file names only against REV-OR-RANGE and optionaly OTHER-REV."
  (interactive
   (list
    (magit-diff-read-range-or-commit "First" "HEAD")
    (magit-diff-read-range-or-commit "Second")))
  (magit-with-toplevel
    (with-current-buffer-window
       (format "diff-files-%s-%s" (first refs) (second refs))
       nil
       nil
     (erase-buffer)
     (--map
      (insert (concat it "\n"))
      (magit-changed-files (first refs) (second refs))))))

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



(defun benj-magit/read-checkout-arg (prompt)
  (magit-read-char-case
      prompt
      t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?s "[s]kip")))

(defun benj-git/resolve-conflicts-interactive (&optional nocs)
  "Interactively resolve all merge conflicts, ask individually.
With non nil prefix arg NOCS, skip cs files."
  (interactive)
  (--map
   (let ((arg (benj-magit/read-checkout-arg
                (format "%s is %s \ncheckout:\n" (car it) (cadr it)))))
     (when arg
       (benj/git-smart-merge-item it arg)))
   (benj-git/unmerged-status (when nocs "*.cs$"))))

(defun benj/git-smart-merge-item (it arg)
  "IT should be an item as returned by `benj-git/unmerged-status', arg is one of \"--ours\" or \"--theirs\".
Attempt to do the right thing for checking out the unmerged file."
  (let ((inhibit-magit-refresh t)
        (ours (string-equal arg "--ours")))
    (pcase (cadr it)
      ("AU"
       (if ours
           (magit-checkout-stage (car it) arg)
         ;; added by us, they don't have a version, so rm
         (magit-call-git "rm" "--" (car it))))
      ("UA"
       ;; added by them,
       ;; actually not sure how you have that, not by deleting on our side, that would be DU.
       ;; I think it only happens with renames maybe
       (if ours
           (magit-call-git "rm" "--" (car it))
         (benj-checkout-stage arg (car it))))
      ("DD" (magit-call-git "rm" "--" (car it)))
      ("DU"
       (if ours
           (magit-call-git "rm" "--" (car it))
         (benj-checkout-stage arg (car it))))
      ("UD"
       (if ours
           (benj-checkout-stage arg (car it))
         (magit-call-git "rm" "--" (car it))))
      ("UU"  (magit-checkout-stage (car it) arg))
      ("AA"  (magit-checkout-stage (car it) arg)))))



(defun benj-git/unmerged-status (&optional exclusion-regex)
  "Get unmerged files in the format defined by `benj-git/git-status-files'.
Ignore file matching EXLUSION-REGEX, it non-nil."
  (let ((unmerged (magit-unmerged-files)))
    (--filter (and  (member (car it) unmerged)
                    (or (null exclusion-regex)
                        (not (string-match-p
                              exclusion-regex
                              (car it)))))
              (benj-git/git-status-files))))

(defun benj-git/git-status-files ()
  "Return a list of unemerged file in the format (FILE STATUS).
For documentation on the status codes see git-status man."
  (let ((default-directory (magit-toplevel)))
    (--map
     (progn (unless (stringp it)
              (error "git status doesn't return a string?"))
            (list (substring it 3 (length it)) (substring it 0 2)))
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

(defmacro benj-git/with-unmerged-files (reg &rest body)
  "Run BODY with the anaphoric 'files'. Filter file names by REG, if REG is nil, don't filter."
  `(progn
     (require 'magit)
     (if-let ((files (--filter (or (not ,reg) (string-match-p ,reg it)) (magit-unmerged-files))))
            (progn ,@body)
          (message "No more unmerged files."))))

(defun benj-magit/ediff-resolve (&optional arg)
  "Start `magit-ediff-resolve' with first unmerged file.
With ARG only check cs files."
  (interactive"P")
  (benj-git/with-unmerged-files
   (and arg "\.cs$")
   (magit-ediff-resolve (first files))))

(defun benj-git/yank-first-unmerged-file (&optional arg)
  "Yank first unmerged file into clipboard using `magit-unmerged-files'.
With ARG only check cs files."
  (interactive"P")
  (benj-git/with-unmerged-files
   (and arg "\.cs$")
   (kill-new (first files))))



(defun benj-git/send-input-to-process (&optional string)
  "Ghetto send some string with newline to git process."
  (interactive"sSend string: ")
  (send-string (get-process "benj-git") (format "%s\n" string)))

;; (defun benj-git/fetch-and-merge ()
;;   "First fetch dev, then merge dev."
;;   ;; TODO something with direnv where we check the primary fetch branch
;;   (interactive)
;;   (benj-run-git-sync "fetch" "origin" "develop:develop")
;;   (benj-run-git "merge" "develop" "--no-edit"))

(defun benj-git/fetch-and-merge ()
  "First fetch dev, then merge dev."
  ;; TODO something with direnv where we check the primary fetch branch
  (require 'magit)
  (interactive)
  (team/magit-pipe
   (magit-run-git-async "fetch")
   (magit-run-git-async
    "fetch" "origin" "develop:develop")
   (magit-merge-plain "develop")))

(defun benj-git/update-modules ()
  "Update modules."
  (require 'magit)
  (interactive)
  (magit-run-git-with-editor "submodule" "update" "--recursive"))

(defun benj-git/reset-modules ()
  "Reset modules."
  (interactive)
  (magit-run-git-async "submodule" "foreach" "git" "reset" "--hard")
  (benj-git/after-magit-success
   (magit-run-git-async "submodule" "foreach" "git" "clean" "-fd")))

(defun benj-git/fire-up-merge-sample (arg)
  "Create an empty git repo, \"git merge topic\"
will create a conflict in a file.
If ARG is nil, try to open an existing merge sample repo, else always create a fresh one."
  (interactive"P")
  (team/with-default-dir
   (or (and (not arg)
            (car
             (process-lines
              "fd"
              "-td"
              "-a"
              "merge-sample"
              "/tmp/")))
       (let ((default-directory
               (concat
                (temporary-file-directory)
                (make-temp-name "merge-sample"))))
      (mkdir default-directory)
      (write-region "" nil "file")
      (shell-command "git init")
      (benj-git//commit-with-msg "Initial commit")
      (shell-command "git checkout -b topic")
      (write-region "best line\n" nil "file")
      (write-region "unrelated file\n" nil "unrelated-file")
      (benj-git//commit-with-msg "Making a change on topic")
      (shell-command "git checkout master")
      (write-region "another line\n" nil "file")
      (benj-git//commit-with-msg "Making a change on master")
      (find-file-other-window "file")
      default-directory))
   (magit-status)))


(defun benj-git//commit-with-msg (msg)
  "Add everything and commit with MSG."
  (shell-command "git add .")
  (shell-command (format "git commit -m \"%s\"" msg)))


;; TODO
(defun benj-git/open-diff-buff-logs-and-todos ()
  "Open buffer for diffs with dev that contain logs or todos."
  (interactive)

  )

(defun benj-git/skip-worktree-file (file)
  "Run git skip worktree with this file."
  (interactive"fFile to skip worktree: ")
  (magit-run-git-async "update-index" "--skip-worktree" "--" file))

(defun benj-git/no-skip-worktree-file (file)
  ""
  (interactive"fFile to no-skip-worktree: ")
  (magit-run-git-async "update-index" "--no-skip-worktree" "--" file))



(defun team/filtered-file-op (arg get-files op)
  "Apply a list of files to OP. GET-FILES should return the initial list of files.
ARG is the filter string to filter files for. It can also be a special number
to use instead
0: .prefab$
1: .meta$
2: .cs$
3: .asset$

If ARG is nil, prompt the user for available extensions intead."
  (let* ((files (funcall get-files))
         (file-match (if arg
                         (pcase arg
                           (0 ".prefab$")
                           (1 ".meta$")
                           (2 ".cs$")
                           (3 ".asset$")
                           (_ arg))
                       (format
                        ".%s$"
                        (completing-read
                         "Extension to act with: "
                         (-uniq
                          (-map
                           #'file-name-extension
                           files)))))))
    (magit-with-toplevel
      (funcall
       op
       (or
        (--filter
         (string-match
          file-match
          it)
         files)
        (user-error "No files like that anymore"))))))


(defmacro team/define-filtered-file-op (name get-files &rest body)
  "Define a function of NAME, use `team/filtered-file-op', wich see.
Eval BODY with anaphoric files set to the filtered files."
  (declare (indent defun))
  (declare (debug t))
  `(defun ,name (arg)
     (interactive"P")
     (team/filtered-file-op
      arg
      ,get-files
      (// (files)
          ,@body))))

(team/define-filtered-file-op
  team/magit-unstage-files
  #'magit-staged-files
  (magit-run-git "checkout" "HEAD" "--" files))

(team/define-filtered-file-op
  team/magit-clean-files
  #'magit-untracked-files
  (mapc #'delete-file files)
  (message "Deleted %d files" (length files)))

(team/define-filtered-file-op
  team/magit-add-files
  #'magit-unstaged-files
  (magit-stage-1 nil files))

(team/define-filtered-file-op
  team/magit-add-untracked
  #'magit-untracked-files
  (magit-stage-1 nil files))

(team/define-filtered-file-op
  team/magit-checkout-changed
  #'magit-unstaged-files
  (magit-run-git "checkout" "HEAD" "--" files))



(defun team/magit-with-files (args files)
  (magit-run-git-async
   `(,@(-flatten args) "--" ,@(mapcar
                               'identity
                               files))))

(defun team/magit-checkout (args files &optional add)
  "ARGS can be nil. FILES is a list of files.
If ADD is non nil, add FILES afterwards."
  (team/magit-with-files
   (list
    "checkout"
    args)
   files)
  (when add
    (benj-git/after-magit-success
     (magit-run-git-async "add" "--" files))))

(defmacro team/magit-define-checkout (name arg)
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     (team/magit-checkout ,arg (magit-unmerged-files))
     (benj-git/after-magit-success
      (magit-stage-1 nil (magit-unmerged-files)))))

(team/magit-define-checkout team/magit-all-theirs
  "--theirs")

(team/magit-define-checkout team/magit-all-ours
  "--ours")

(defun team/magit-all-merge ()
  (interactive)
  (team/magit-checkout "--merge" (magit-unmerged-files)))


(defun team/magit-restore-merge (&optional file)
  (interactive"f")
  (team/magit-checkout "--merge" (list file)))



(defmacro team/with-buff-file-protected (&rest body)
  "Run BODY with anaphoric it bound to the buffer file name.
If buffer is not visiting a file, log an user error."
  (declare (debug body))
  `(team/a-if (buffer-file-name)
             ,@body
             (user-error "Buffer is not visiting a file.")))

(defun team/magit-log-rev ()
  "Use magit to log for current file and read which rev."
  (interactive)
  (team/with-buff-file-protected
   (let ((magit-buffer-refname
          (magit-read-branch-prefer-other (format "%s: log" (file-name-nondirectory it)))))
     (magit-log-buffer-file))))

(defun team/magit--read-left-right ()
  (format "%s..%s" (magit-read-branch-or-commit "Left") (magit-read-branch-or-commit "Right" "HEAD")))

(defun team/magit-log-double-dot ()
  "Setup magit buffer with double dot revision range. Prompt user for left and right."
  (interactive)
  (magit-log-setup-buffer
   (list (team/magit--read-left-right))
   nil nil))

(defun team/magit-log-double-dot-file ()
  "Log buffer file with double dot revision range."
  (interactive)
  (team/with-buff-file-protected
   (magit-log-setup-buffer
    (list (team/magit--read-left-right))
    nil (team/mklist it))))


;; doesn't work
;; (defun team/magit-log-range ()
;;   "Use magit to log for current file, ask for revs to log with REV..REV."
;;   (interactive)
;;   (team/with-buff-file-protected
;;    (magit-log-setup-buffer
;;     (list
;;      (format "%s..%s"
;;              (magit-read-branch "Log, left rev" "develop")
;;              (magit-read-branch "Log, right rev" "HEAD")))
;;     '("--graph" "-n256" "--decorate" "--merge" "--no-merges")
;;     (team/mklist it))))




(defun team/magit-fetch-any (&optional arg)
  "Read branch and fetch upstream into local.
With ARG, default to 'develop'."
  (interactive)
  (require 'magit)
  (let ((branch-name
         (if arg
             "develop"
           (team/re-replace-in-string
            (magit-read-branch-prefer-other
             "Branch to fetch")
            "origin"
            ""))))
    (magit-run-git-async
     "fetch"
     (or (magit-get-upstream-remote branch-name) "origin")
     (format "%1$s:%1$s" branch-name))))


(defun team/magit-rebase-onto (target)
  (interactive
   (list (magit-read-other-branch-or-commit "Rebase onto")))
  (magit-rebase-branch target (list "--autosquash" "--autostash" "-i")))

(defun team/list-current-unmerged-files ()
  "Show current unmerged files in a window"
  (interactive)
  (magit-with-toplevel
    (team/show-in-window (magit-unmerged-files) "unmerged")
    (when (file-in-directory-p default-directory cos-dir)
      (require 'benj-funcs)
      (cos/write-conflicted-prefabs-to-file))))

(defun team/git-status-s ()
  "Put the output of git status -s into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process
     "git"
     nil
     (current-buffer)
     nil
     "status"
     "-s")
    (re-search-backward "^$")
    (delete-region
     (point)
     (point-max))
    (->gg)
    (while (re-search-forward "^\\w  " nil t)
      (team/delete-this-line))
    (->gg)
    ;; (switch-to-buffer (current-buffer))
        ;; (switch-to-buffer (current-buffer))
       ;; (while (re-search-forward "\\(^\\w\\w \\)" nil t)
       ;;   (let ((o (make-overlay
       ;;             (match-beginning 0)
       ;;             (match-end 0))))
       ;;     (overlay-put o 'invisible t)))
    ))

(defun team/git-status-s-window ()
  (interactive)
  (with-current-buffer-window
      (get-buffer-create "unmerged-status")
      nil
      nil
    (team/git-status-s)))


(defun team/checkout-checkout-files-on-region ()
  "Checkout some selected files."
  (interactive)
  (let ((files
         (-remove #'string-empty-p (s-split "\n" (team/line-or-region-str)))))
    (team/magit-checkout
     (benj-magit/read-checkout-arg (format "checkout %d files: " (length files)))
     files
     'add)))

(defun team/show-in-window (list &optional buff-name)
  "Flatten LIST and insert into a temp window.
Optionally set BUFF-NAME or default to  'out'"
  (with-current-buffer-window
      (or buff-name "out")
      nil
      nil
    (--map
     (insert (concat it "\n"))
     (-flatten list))))




(defun team/magit-read-unmerged (prompt)
  (team/magit--read-file prompt #'magit-unmerged-files t))

(defun team/magit--read-file (prompt file-op &optional tracked-only)
  "See `magit-read-file'"
  (let ((choices (nconc (funcall file-op)
                        (unless tracked-only (magit-untracked-files)))))
    (magit-completing-read
     prompt choices nil t nil nil
     (car (member (or (magit-section-value-if '(file submodule))
                      (magit-file-relative-name nil tracked-only))
                  choices)))))


(defun team/magit-log-merge (&optional files)
  "Show log for merge conflicts"
  (interactive
   (list (team/magit-read-unmerged "Unmerged file to log merge")))
  (magit-log-setup-buffer (list "HEAD") '("--graph" "-n256" "--decorate" "--merge" "--no-merges") (team/mklist files)))




;; smoother checkout, annoying metas or asset files

(defun team-magit/insert-last-proc-out ()
  "Insert the last output section of `magit-process-buffer'"
  (magit-with-toplevel
    (insert
    (with-current-buffer
        (magit-process-buffer t)
      (let ((section (car (last (oref magit-root-section children)))))
        (buffer-substring (oref section start) (oref section end)))))))


(defun team-magit/collect--blocking-files (initial-reg end-reg)
  "Return a list of files parsed from git output when it tells you that files
would be overriden or by a checkout.
INITIAL-REG is the beginning part of the git msg, END-REG is the end part of the message."
  (->gg)
  (when (re-search-forward initial-reg nil t)
    (let ((res))
      (while (re-search-forward "^\t\\(.*\\)$"
                                (save-excursion
                                  (re-search-forward end-reg nil t))
                                t)
        (push (match-string-no-properties 1) res))
      res)))

(defun team-magit/collect--git-cmd ()
  "Return a list of string which are a magit git cmd args
from the first line current buffer.
Assume syntax as in `magit-process-buffer'."
  (split-string
   (buffer-substring-no-properties
    (progn
      (->gg)
      (skip-chars-forward
       (concatenate 'string "^" (make-string 1 #x2026)))
      (forward-char 1)
      (point))
    (point-at-eol))))

(defconst
  team-magit/checkout-or-merge-word
  "\\(\\(checkout\\)\\|\\(merge\\)\\):")

(defun team-magit/checkout-annoying ()
  "Check the last magit output section.
If there is a git message about changes to files that would be overriden, checkout those files. Else if the message talks about untracked files, delete them, if they are metas, else prompt the user for confirmation."
  (interactive)
  (with-temp-buffer
    (team-magit/insert-last-proc-out)
    (let ((checkout-them
           (team-magit/collect--blocking-files
            (concat "error: Your local changes to the following files would be overwritten by " team-magit/checkout-or-merge-word)
            "Please commit your changes "))
          (delete-them
           (team-magit/collect--blocking-files
            (concat "error: The following untracked working tree files would be overwritten by " team-magit/checkout-or-merge-word)
            "Please move or remove them before you switch branches.")))
      (unless (or checkout-them delete-them)
        (user-error "Did not find any files to operate on."))
      (magit-with-toplevel
        (when checkout-them
          (magit-run-git
           "checkout" "--"
           checkout-them))
        (when delete-them
          (--map
           (when
               (or (string-match-p ".*meta$" it)
                   (yes-or-no-p (format "Do you want to delete %s?" it)))
             (delete-file it))
           delete-them)))
      (magit-run-git-async (team-magit/collect--git-cmd)))))

(defun team/magit-changed-files-reg (reg rev-or-range &optional other-rev)
  "Return the changed files changed between REV-OR-RANGE matching REG.
OTHER-REV defaults to HEAD."
  (--filter (string-match-p reg it)
            (magit-changed-files rev-or-range other-rev)))

(defun team/magit-changed-prefabs (rev-or-range other-rev)
  (team/magit-changed-files-reg ".*\.prefab$" rev-or-range other-rev))



(defun benj/magit-show-commit-no-limit-files ()
  "Like RET in a log buffer but do not limit to the log files."
  (require 'magit)
  (interactive)
  (let ((magit-buffer-log-files nil))
    (call-interactively #'magit-show-commit nil nil)))

(defun benj/magit-log-file-no-merges ()
  (interactive)
  (let ((magit-buffer-log-args))
    (magit-log-setup-buffer
     (list "HEAD")
     (list "--no-merges")
     (list (buffer-file-name)))))

(defun benj/magit-log-file-me ()
  (interactive)
  (let ((magit-buffer-log-args))
    (magit-log-setup-buffer
     (list "HEAD")
     (list "--no-merges" "--author=Benj")
     (list (buffer-file-name)))))



(defun my/git-gutter+-revert-hunks-no-ask ()
  "See `git-gutter+-revert-hunks'."
  (interactive)
  (let* ((diffinfos (git-gutter+-selected-diffinfos))
         (one-diffinfo-p (= 1 (length diffinfos))))
    (dolist (diffinfo (nreverse diffinfos))
      (git-gutter+-do-revert-hunk diffinfo))
    (save-buffer)
    (if one-diffinfo-p
        (--when-let (get-buffer git-gutter+-popup-buffer)
          (kill-buffer it)))))



(defun benj-git/reset-head-1 ()
  "Hard reset to HEAD~1."
  (interactive)
  (let ((inhibit-magit-refresh t))
    (magit-run-git-async
     "reset"
     "--hard"
     "HEAD~1")))



(defun benj/read-recent-commit ()
  (interactive)
  (completing-read
   "Insert recent commit: "
   (with-temp-buffer
     (magit-insert-recent-commits)
     (cdr (s-split "\n" (buffer-string))))))

(defun benj/read-project-dir ()
  "Use projectile to read a project."
  (interactive)
  (list
   (let ((projects (projectile-relevant-known-projects)))
     (if projects
         (completing-read
          "Project for commit: " projects)
       (user-error "There are no known projects")))))

(defun benj/format-magit-commit (commit-s)
  (-some-->
      commit-s
    (s-split-up-to " " it 1)
    (format
     "`%s` \n> %s\n"
     (car it)
     (cadr it))))

(defun benj/insert-recent-commit (&optional dir)
  "Read a recent commit and insert formatted. DIR should be a directory in a git repo."
  (interactive
   (benj/read-project-dir))
  (insert
   (benj/format-magit-commit
    (team/with-default-dir
     dir
     (benj/read-recent-commit)))))

(defun benj/copy-recent-commit-in-file (&optional dir)
  "Kill commit text suitable for team communication. Read from recent commits in visited file.
DIR should be a directory in a git repo."
  (interactive
   (benj/read-project-dir))
  (kill-new
   (benj/format-magit-commit
    (team/with-default-dir
     dir
     (let
         ((magit-buffer-log-args
           (list
            "--author=Benj"))
          (file (buffer-file-name)))
       (completing-read
        "Copy recent commit: "
        (with-temp-buffer
          (magit-insert-log
           "HEAD"
           (cons (format "-n%d" 20)
                 (--remove (string-prefix-p "-n" it)
                           magit-buffer-log-args))
           (list file))
          (cdr (s-split "\n" (buffer-string))))))))))

(defun benj/insert-recent-cos-commit ()
  (interactive)
  (benj/insert-recent-commit cos-dir))

(defun benj/copy-recent-cos-commit-in-file ()
  (interactive)
  (benj/copy-recent-commit-in-file cos-dir))

(provide 'benj-magit)
