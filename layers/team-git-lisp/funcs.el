;;(require 'magit)

;;; Code:

(defvar-local benj-git/after-magit-op nil)
(defun benj-git/after--magit (op)
  "Call OP after magit process finished with a single arg, the exit status."
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




(defun team/magit-is-ancestor ()
  "Use magit to read 2 revs, print message if A is ancestor of B."
  (interactive)
  (let ((a (magit-read-branch-or-commit "Ancestor"))
         (b (magit-read-branch-or-commit "Descendant")))
    (message
     "%s is %sancestor of %s"
     a
     (if (magit-rev-ancestor-p a b)
         ""
       "not ")
     b)))

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



(defun benj-git/resolve-conflicts-interactive (&optional nocs)
  "Interactively resolve all merge conflicts, ask individually.
With non nil prefix arg NOCS, skip cs files."
  (interactive)
  (--map
   (let* ((arg (magit-read-char-case
                   (format "%s is %s \ncheckout:\n" (car it) (cadr it))
                   t
                 (?o "[o]ur stage"   "--ours")
                 (?t "[t]heir stage" "--theirs")
                 (?s "[s]kip")))
          (ours (string-equal arg "--ours")))
     (when arg
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
        ("UU"  (benj-checkout-stage arg (car it)))
        ("AA"  (benj-checkout-stage arg (car it))))))
   (benj-git/unmerged-status (when nocs "*.cs$"))))

(defun benj-git/unmerged-status (&optional exclusion-regex)
  "Get unmerged files in the format defined by `benj-git/git-status-files'.
Ignore file matching EXLUSION-REGEX, it non-nil."
  (let ((unmerged (magit-unmerged-files)))
    (--filter (and  (member (car it) unmerged)
                    (or (null exclusion-regex)
                        (not (string-match-p exclusion-regex it))))
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

(defmacro benj-git/with-unmerged-files (&rest body)
  "Run BODY with the anaphoric 'files'."
  `(progn
     (require 'magit)
      (if-let ((files (magit-unmerged-files)))
            (progn ,@body)
          (message "No more unmerged files."))))

(defun benj-magit/ediff-resolve ()
  "Start `magit-ediff-resolve' with first unmerged file."
  (interactive)
  (benj-git/with-unmerged-files
   (magit-ediff-resolve (first files))))

(defun benj-git/yank-first-unmerged-file ()
  "Yank first unmerged file into clipboard using `magit-unmerged-files'"
  (interactive)
  (benj-git/with-unmerged-files
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
  (magit-run-git-async
   "fetch" "origin" "develop:develop")
  (benj-git/after-magit-success
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

(defun benj-git/fire-up-merge-sample ()
  "Create an empty git repo, should put you in a state where doing \"git merge topic\"
will create a conflict in a file called file."
  (interactive)
  (let ((dir (concat (temporary-file-directory) (make-temp-name "merge-sample"))))
    (mkdir dir)
    (let ((default-directory dir))
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
      (find-file "file")
      (magit-status))))


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
3: .asset$"
  (unless arg (user-error "Arg a string or one of
0: .prefab$
1: .meta$
2: .cs$
3: .asset$"))
  (magit-with-toplevel
    (funcall
     op
     (or
      (--filter
       (string-match
        (pcase arg
          (0 ".prefab$")
          (1 ".meta$")
          (2 ".cs$")
          (3 ".asset$")
          (_ arg))
        it)
       (funcall get-files))
      (user-error "No files like that anymore")))))

(defmacro team/define-filtered-file-op (name get-files &rest body)
  "Define a function of NAME, use `team/filtered-file-op', wich see.
Eval BODY with anaphoric files set to the filtered files."
  (declare (indent defun))
  (declare (debug body))
  `(defun ,name (arg)
     (interactive"P")
     (team/filtered-file-op
      arg
      ,get-files
      (// (files)
          ,@body))))

(defun team/magit-with-files (args files)
  (magit-run-git-async
   `(,@(-flatten args) "--" ,@(mapcar
                                  'identity
                                  files))))

(defun team/magit-checkout (args files)
  "ARGS can be nil. FILES is a list of files."
  (team/magit-with-files
   (list
    "checkout"
    args)
   files))



;; (defun team/)

(defun team/magit-checkout-head (files)
  (team/magit-checkout-head nil files))

(team/define-filtered-file-op
  team/magit-unstage-files
  #'magit-staged-files
  #'team/magit-checkout-head)

(team/define-filtered-file-op
  team/magit-clean-files
  #'magit-untracked-files
  (mapc #'delete-file files)
  (message "Deleted %d files" (length files)))

(team/define-filtered-file-op
  team/magit-add-untracked
  #'magit-untracked-files
  (magit-run-git
   "add"
   "--"
   files))

(team/define-filtered-file-op
  team/magit-checkout-changed
  (lambda () (magit-changed-files "HEAD"))
  #'team/magit-checkout-head)



(defun team/magit-log-rev ()
  (interactive)
  (team/a-if (buffer-file-name)
             (let ((magit-buffer-refname
                    (magit-read-branch-prefer-other (format "%s: log:" (file-name-nondirectory it)))))
               (magit-log-buffer-file))
             (user-error "Buffer is not visiting a file.")))




(defun team/magit-add-unmerged ()
  (team/magit-with-files
   "add"
   (magit-unmerged-files)))

(defmacro team/magit-define-checkout (name arg)
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     (team/magit-checkout ,arg (magit-unmerged-files))
     (benj-git/after-magit-success
      (team/magit-add-unmerged))))


(team/magit-define-checkout team/magit-all-theirs
  "--theirs")

(team/magit-define-checkout team/magit-all-ours
  "--ours")

(team/magit-define-checkout team/magit-all-merge
  "--merge")







(defun team/magit-fetch-any (&optional arg)
  "Read branch and fetch upstream into local.
With ARG, default to 'develop'."
  (interactive)
  (require 'magit)
  (let ((branch-name
         (if arg
             "develop"
           (magit-read-branch-prefer-other
            "Branch to fetch"))))
    (magit-run-git-async
     "fetch"
     (magit-get-upstream-remote branch-name)
     (format "%1$s:%1$s" branch-name))))


(defun team/magit-rebase-onto (target)
  (interactive
   (list (magit-read-other-branch-or-commit "Rebase onto")))
  (magit-rebase-branch target (list "--autosquash" "--autostash" "-i")))

(defun team/list-current-unmerged-files ()
  "Show current unmerged files in a window"
  (interactive)
  (magit-with-toplevel
    (team/show-in-window (magit-unmerged-files) "unmerged")))

(defun team/list-current-unmerged-status ()
  (interactive)
  (magit-with-toplevel
    (team/show-in-window
     (--map
      (list (format "%s %s" (cadr it) (car it)))
      (benj-git/unmerged-status))
     "unmerged-status")))


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
   (list (team/magit-read-unmerged "Unmerged file to log merge:")))
  (magit-log-setup-buffer (list "HEAD") '("--graph" "-n256" "--decorate" "--merge" "--no-merges") (team/mklist files)))
