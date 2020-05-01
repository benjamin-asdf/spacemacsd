;; funcs.el starts here;; funcs.el starts here
(defun mikus-reopen-buffer ()
  "Kill and open current BUFFER."
  (interactive)
  (let ( (buffer (buffer-name))
         (file (buffer-file-name))
         (point (point)) )
    (kill-buffer buffer)
    (find-file file)
    (goto-char point)))

(defun benj-copy-word-from-above ()
  "Copy the word or space from next non-empty line above."
  (interactive)
  (let ((word)
        (col (current-column))
        (line (1- (line-number-at-pos))))
    (while (and (not (= line 0))
            (not (setq word (benj--word-in-column line col))))
      (setq line (1- line)))
    (when word
      (kill-new word)
      (yank))))

(defun benj--word-in-column (line col)
  "Evaluates to the evil word in LINE and COL.
 single space if char at point is a space. Nil for empty lines."
  (save-excursion
    (goto-line line)
    (evil-goto-column col)
    (cond ((looking-at "^$") nil)
          ((string-equal (thing-at-point 'char) " ") " ")
          (t (thing-at-point 'evil-word)))))


(defun benj-read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun benj-flush-empty-lines ()
  "Delete empty lines on selection."
  (interactive)
  (flush-lines "^$" (region-beginning) (region-end)))

(defun benj-insert-other-line ()
  "Go into insert mode in the line below.
Use correct indentation. Like 'o' without creating a new line"
  (interactive)
  (forward-line)
  (evil-insert-state)
  (indent-according-to-mode))

(defun benj-delete-all-files (dir)
  "Delete all files inside DIR."
  (dolist (elem (directory-files dir))
    (unless (member elem '("." ".."))
      (delete-file (concat (file-name-as-directory dir) elem)))))

(defun benj-best-message ()
  "A random line chosen from best-message file."
  (benj-rand-line-from-file best-messages-file))

(defun benj-rand-line-from-file (file)
  "A random line from FILE"
  (rand-element (benj-read-lines file)))

(defun rand-element (list)
  "Random element from LIST."
  (nth (random (length list)) list))

(defun benj-new-python-script (file)
  "Create a new python script shell script with NAME."
  (interactive "FNew script: ")
  (benj--new-script-worker file "#!/usr/bin/env python3"))

(defun benj-new-shell-script (file)
  "Create a new script shell script with NAME."
  (interactive "FNew script: ")
  (benj--new-script-worker file "#!/bin/sh\n\n"))

(defun benj--new-script-worker (file shebang)
  "Insert SHEBANG into FILE and make it executable"
  (unless (file-exists-p file)
    (write-region shebang " " file))
  (set-file-modes file #o777)
  (find-file file)
  (goto-char (point-max))
  (evil-insert-state))


(defun benj-process-other-window (process-name buffer-name process-program &rest process-args)
  "Start process and switch to output buffer in other window."
  (start-process process-name buffer-name process-program (mapconcat 'identity process-args " "))
  (unless (string-equal (buffer-name) buffer-name)
    (switch-to-buffer-other-window buffer-name)))

(defun benj-append-to-file (file content &optional newline)
  "Append a newline with CONTENT to FILE.
If NEWLINE is non nil, append a newline character."
  (unless (file-exists-p file)
    (write-region "" nil file))
  (with-temp-file file
    (insert-file-contents file)
    (goto-char (point-max))
    (insert (if newline (concat content "\n") content))))

(with-eval-after-load 'projectile
  (defconst benj-fd-find-alot "fd -H -E=.git -I -tf -0 .")

  (defun benj-helm-projectile-find-many-files ()
    "Simple implementation of projectile find file without any file filters"
    (interactive)
    (let* ((project-root (projectile-ensure-project (projectile-project-root)))
           (files (projectile-files-via-ext-command project-root benj-fd-find-alot))
           (file (projectile-completing-read "Find file: "
                                             files)))
      (when file
        (funcall #'find-file (expand-file-name file project-root))
        (run-hooks 'projectile-find-file-hook))))

  ;; ;;TODO
  ;; (defun benj-helm-projectile-find-files-in-dir ()
  ;; ;;   "Search only specific dir"

  ;;   (interactive)
  ;;   (let*
  ;;       ;; TODO
  ;;       (
  ;;        (benj-fd-find-alot "fd -I -tf -0 -e cs . /home/benj/idlegame/IdleGame/Assets/#/Design/")
  ;;        (project-root (projectile-ensure-project (projectile-project-root)))
  ;;        (files (projectile-files-via-ext-command project-root benj-fd-find-alot))
  ;;        (file (projectile-completing-read "Find file in design folder: "
  ;;                                          files)))
  ;;     (when file
  ;;       (funcall #'find-file (expand-file-name file project-root))
  ;;       (run-hooks 'projectile-find-file-hook))))



  (defun benj-diff-files (&optional rev)
    "Print the output of git diff --name-only to temp buffer.
If REV is non nil compare with REV instead of default develop."
    (interactive)
    (let ((other-rev (or rev "develop")))
      (with-output-to-temp-buffer (format "diff-%s..HEAD" other-rev)
        (print (benj--git-diff-files other-rev))
        (print standard-output))))


  ;; (defun benj-ghetto-run-git-push ()
  ;;   (interactive)
  ;;   (let ((default-directory (projectile-ensure-project (projectile-project-root))))
  ;;     (shell-command "git push")))

  (defun benj-curr-revision-as-kill (branch-name auto-insert)
    "Copy current git revision as kill.
If BRANCH-NAME is non nil, copy the branch name instead of commit sha.
If AUTO-INSERT is non nil, instantly insert at current buffer position."
  (let ((output (benj-curr-revision branch-name))
        (message output))
    (when auto-insert (insert output))
    (kill-new output)))

  (defun benj-curr-revision (&optional branch-name)
    "Current git revision. If BRANCH-NAME is non nil, evaluate to the branch name instead of the commit sha."
    (benj-remove-newline-end-of-string
     (benj-projectile-dir-command-to-string
      (if branch-name "git branch --show-current" "git rev-parse HEAD"))))

  (defun benj-projectile-dir-command-to-string (command)
    "Run COMMAND with the current projectile project root as default dir.
Evaluate to the output string. See `shell-command-to-string'."
    (let ((default-directory (projectile-ensure-project (projectile-project-root))))
      (message (format "run command: %s in dir: %s" command default-directory))
      (shell-command-to-string command)))

  (defun benj-make-merge-request-commit ()
    "Run 'git commit --allow-empty -m '\do merge' in the current project dir"
    (interactive)
    (benj-projectile-dir-command-to-string "git commit --allow-empty -m \"do /merge\""))

  ;; NOTE way more zappy than magit
  (defun benj-quick-commit (arg)
    "Run git commit in projectile root with ARG as commit message"
    (interactive "sCommit Message: ")
    (benj-projectile-dir-command-to-string (format "git commit -m \"%s\"" arg)))


  (defun benj--git-diff-files (rev1 &optional rev2)
    "Get shell output for git diff files of REV1 agains REV2, if REV2 is ommitted, default to HEAD."
    (benj-projectile-dir-command-to-string (format "git diff --name-only %s..%s" rev1 (or rev2 "HEAD"))))

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

  (defun benj--get-diff-output-match-lines (regex rev1 &optional rev2)
    "Evaluate to a string containg the file names and lines matching REGEX
  in the git diff output of REV1 against REV2,
  if REV2 is ommitted it defaults to HEAD.
  this also sets the return value of `match-string'."
    ;; TODO line number would be cool
    (let ((ret)
          (files (benj--git-diff-files-list rev1 rev2)))
      (dolist (file files)
        (benj--log-to-diff-output (format "%s %d%%" file (/ (cl-position file files) (* (length files) 1.0)) 100))
        (let ((match (benj--file-diff-string-match file regex rev1 rev2)))
          (when match (setq ret (concat ret "\n" (format "File:%s : %s" file match) ret)))))
      (or ret (format "No line differences found for %s against %s" regex rev1 rev2)))

    )

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
    (benj--diff-output-match-lines "^+.*Debug.Log(.*).*$" "develop")))


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

(defun benj--git-diff-files-list (rev1 &optional rev2)
  "Get git diff files of REV1 against REV2, if REV2 is ommitted, default to HEAD."
  (split-string (benj--git-diff-files rev1 rev2)))


(defun benj-copy-last-yank-to-register (&optional reg)
  "Copy the contens of the \" register into REG.
Default is register a."
  (interactive)
  (evil-set-register (or reg ?a) (evil-get-register ?\" t)))


;; TODO fix case where there are no lines above us here
(defun benj-delete-some-whitespace ()
  "Delete a lot of white space but keep one line.
This is the same as vim `dipO'"
  (interactive)
  (re-search-backward "^.+$")
  (delete-blank-lines)
  (forward-line 1)
  (insert "\n"))

(defun benj-copy-file-pos-pretty ()
  "Copy the current pos in the format <filename> line <linenum>."
  (interactive)
  (kill-new
   (format "`%s` line %d"
           (file-name-base (buffer-file-name))
           (line-number-at-pos (point)))))

(defun benj-delete-til-closing-parem ()
  "Delete the rest until closing parem.
Basically evil `dt)'"
  (interactive)
  (let ((beg (point)))
    (search-forward ")")
    (forward-char -1)
    (delete-region beg (point))))

(defun benj-directory-files (path)
  "Get directory files from PATH. Excludes '.' and '..'."
  (let ((ret '()))
    (dolist (file (directory-files path t) ret)
      (unless (member (file-name-nondirectory file) '("." ".."))
        (setq ret (cons file ret))))
    ret))


(defun benj-clear-directory-contents (path)
  "Delete all files inside directory PATH."
  (dolist (file (benj-directory-files path))
    (delete-file file)))


;; temp
(defun benj-change-best-img ()
  (interactive)
  (search-forward-regexp "\\w+ Image \\(\\w+\\)")
  (message (match-string 1))
  (goto-char (line-end-position))
  (insert (format "\n     public BestImage best_%s" (match-string 1))))

(defun benj-mean (lst)
  (/ (float (apply '+ lst)) (length lst)))



(defconst benj-scratch-buffer-kinds
  '((:csharp . csharp-mode)
    (:fundamental . fundamental-mode)
    (:lisp-interaction . lisp-interaction-mode)
    (:markdown . markdown-mode)
    (:org . org-mode))
  "Map scratch buffer kind names with respective mode.
Form '(:key . MODE-FUNC)")

(defun benj--switch-to-scratch-buffer (arg)
  "Switch to one of the `'*scratch<name>*' buffers.
ARG should be one of `benj-scratch-buffer-kinds'"
  (let* ((buff-name (format "*scratch%s*" arg))
         (exists (get-buffer buff-name))
         (mode (cdr (assoc arg benj-scratch-buffer-kinds))))
    (switch-to-buffer (get-buffer-create buff-name))
    (when (and (not exists)
               (not (eq major-mode mode))
               (fboundp mode))
      (funcall mode))))





(defun benj-next-digit ()
  "Jump to next digit in buff"
  (interactive)
  (re-search-forward "[0-9]+")
  (forward-char -1))





;; TODO
(defun benj-rename-file-and-meta ()
  ;;
  )


(defun test-interactive (file-name)
  (interactive
   (let*
       ((default (thing-at-point 'evil-word))
        (str (read-string (format "New file name (default: %s)" default) nil nil default)))
     (list str)))
  (message file-name)
  )



(defun benj-remove-eol (file)
  "Remove crlf from FILE."
  (interactive"fRemove eol from file: ")
  (with-temp-file file
    (insert-file-contents-literally file)
    (while (re-search-forward "\r\n" nil t) (replace-match "\n"))))

























;; TEMP hack

;; original definition is in

(defun dir-locals-read-from-dir (dir)
  "Load all variables files in DIR and register a new class and instance.
DIR is the absolute name of a directory, which must contain at
least one dir-local file (which is a file holding variables to
apply).
Return the new class name, which is a symbol named DIR."
  (let* ((class-name (intern dir))
         (files (dir-locals--all-files dir))
	 ;; If there was a problem, use the values we could get but
	 ;; don't let the cache prevent future reads.
	 (latest 0) (success 0)
         (variables))
    (with-demoted-errors "Error reading dir-locals: %S"
      (dolist (file files)
	(let ((file-time (file-attribute-modification-time
			  (file-attributes file))))
	  (if (time-less-p latest file-time)
	    (setq latest file-time)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((newvars
                 (condition-case-unless-debug nil
                     ;; As a defensive measure, we do not allow
                     ;; circular data in the file/dir-local data.
                     (let ((read-circle nil))
                       (read (current-buffer)))
                   (end-of-file nil))))
            (setq variables
                  ;; Try and avoid loading `map' since that also loads cl-lib
                  ;; which then might hamper bytecomp warnings (bug#30635).
                  (if (not (and newvars variables))
                      (or newvars variables)
                    (require 'map)
                    (map-merge-with 'list (lambda (a b) (map-merge 'list a b))
                                    variables
                                    newvars))))))
      (setq success latest))
    (if (not (listp variables)) (setq variables '()))
    (setq variables (dir-locals--sort-variables variables))
    (dir-locals-set-class-variables class-name variables)
    (dir-locals-set-directory-class dir class-name success)
    class-name))

(defun mikus-reopen-buffer ()
  "Kill and open current BUFFER."
  (interactive)
  (let ( (buffer (buffer-name))
         (file (buffer-file-name))
         (point (point)) )
    (kill-buffer buffer)
    (find-file file)
    (goto-char point)))

(defun benj-copy-word-from-above ()
  "Copy the word or space from next non-empty line above."
  (interactive)
  (let ((word)
        (col (current-column))
        (line (1- (line-number-at-pos))))
    (while (and (not (= line 0))
            (not (setq word (benj--word-in-column line col))))
      (setq line (1- line)))
    (when word
      (kill-new word)
      (yank))))

(defun benj--word-in-column (line col)
  "Evaluates to the evil word in LINE and COL.
 single space if char at point is a space. Nil for empty lines."
  (save-excursion
    (goto-line line)
    (evil-goto-column col)
    (cond ((looking-at "^$") nil)
          ((string-equal (thing-at-point 'char) " ") " ")
          (t (thing-at-point 'evil-word)))))


(defun benj-read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun benj-flush-empty-lines ()
  "Delete empty lines on selection."
  (interactive)
  (flush-lines "^$" (region-beginning) (region-end)))

(defun benj-insert-other-line ()
  "Go into insert mode in the line below.
Use correct indentation. Like 'o' without creating a new line"
  (interactive)
  (forward-line)
  (evil-insert-state)
  (indent-according-to-mode))

(defun benj-delete-all-files (dir)
  "Delete all files inside DIR."
  (dolist (elem (directory-files dir))
    (unless (member elem '("." ".."))
      (delete-file (concat (file-name-as-directory dir) elem)))))

(defun benj-best-message ()
  "A random line chosen from best-message file."
  (benj-rand-line-from-file best-messages-file))

(defun benj-rand-line-from-file (file)
  "A random line from FILE"
  (rand-element (benj-read-lines file)))

(defun rand-element (list)
  "Random element from LIST."
  (nth (random (length list)) list))

(defun benj-new-python-script (file)
  "Create a new python script shell script with NAME."
  (interactive "FNew script: ")
  (benj--new-script-worker file "#!/usr/bin/env python3"))

(defun benj-new-shell-script (file)
  "Create a new script shell script with NAME."
  (interactive "FNew script: ")
  (benj--new-script-worker file "#!/bin/sh\n\n"))

(defun benj--new-script-worker (file shebang)
  "Insert SHEBANG into FILE and make it executable"
  (unless (file-exists-p file)
    (write-region shebang " " file))
  (set-file-modes file #o777)
  (find-file file)
  (goto-char (point-max))
  (evil-insert-state))


(defun benj-process-other-window (process-name buffer-name process-program &rest process-args)
  "Start process and switch to output buffer in other window."
  (start-process process-name buffer-name process-program (mapconcat 'identity process-args " "))
  (unless (string-equal (buffer-name) buffer-name)
    (switch-to-buffer-other-window buffer-name)))

(defun benj-append-to-file (file content &optional newline)
  "Append a newline with CONTENT to FILE.
If NEWLINE is non nil, append a newline character."
  (unless (file-exists-p file)
    (write-region "" nil file))
  (with-temp-file file
    (insert-file-contents file)
    (goto-char (point-max))
    (insert (if newline (concat content "\n") content))))

(with-eval-after-load 'projectile
  (defconst benj-fd-find-alot "fd -H -E=.git -I -tf -0 .")

  (defun benj-helm-projectile-find-many-files ()
    "Simple implementation of projectile find file without any file filters"
    (interactive)
    (let* ((project-root (projectile-ensure-project (projectile-project-root)))
           (files (projectile-files-via-ext-command project-root benj-fd-find-alot))
           (file (projectile-completing-read "Find file: "
                                             files)))
      (when file
        (funcall #'find-file (expand-file-name file project-root))
        (run-hooks 'projectile-find-file-hook))))

  ;; ;;TODO
  ;; (defun benj-helm-projectile-find-files-in-dir ()
  ;; ;;   "Search only specific dir"

  ;;   (interactive)
  ;;   (let*
  ;;       ;; TODO
  ;;       (
  ;;        (benj-fd-find-alot "fd -I -tf -0 -e cs . /home/benj/idlegame/IdleGame/Assets/#/Design/")
  ;;        (project-root (projectile-ensure-project (projectile-project-root)))
  ;;        (files (projectile-files-via-ext-command project-root benj-fd-find-alot))
  ;;        (file (projectile-completing-read "Find file in design folder: "
  ;;                                          files)))
  ;;     (when file
  ;;       (funcall #'find-file (expand-file-name file project-root))
  ;;       (run-hooks 'projectile-find-file-hook))))



  (defun benj-diff-files (&optional rev)
    "Print the output of git diff --name-only to temp buffer.
If REV is non nil compare with REV instead of default develop."
    (interactive)
    (let ((other-rev (or rev "develop")))
      (with-output-to-temp-buffer (format "diff-%s..HEAD" other-rev)
        (print (benj--git-diff-files other-rev))
        (print standard-output))))


  ;; (defun benj-ghetto-run-git-push ()
  ;;   (interactive)
  ;;   (let ((default-directory (projectile-ensure-project (projectile-project-root))))
  ;;     (shell-command "git push")))

  (defun benj-curr-revision-as-kill (branch-name auto-insert)
    "Copy current git revision as kill.
If BRANCH-NAME is non nil, copy the branch name instead of commit sha.
If AUTO-INSERT is non nil, instantly insert at current buffer position."
  (let ((output (benj-curr-revision branch-name))
        (message output))
    (when auto-insert (insert output))
    (kill-new output)))

  (defun benj-curr-revision (&optional branch-name)
    "Current git revision. If BRANCH-NAME is non nil, evaluate to the branch name instead of the commit sha."
    (benj-remove-newline-end-of-string
     (benj-projectile-dir-command-to-string
      (if branch-name "git branch --show-current" "git rev-parse HEAD"))))

  (defun benj-projectile-dir-command-to-string (command)
    "Run COMMAND with the current projectile project root as default dir.
Evaluate to the output string. See `shell-command-to-string'."
    (let ((default-directory (projectile-ensure-project (projectile-project-root))))
      (message (format "run command: %s in dir: %s" command default-directory))
      (shell-command-to-string command)))

  (defun benj-make-merge-request-commit ()
    "Run 'git commit --allow-empty -m '\do merge' in the current project dir"
    (interactive)
    (benj-projectile-dir-command-to-string "git commit --allow-empty -m \"do /merge\""))

  ;; NOTE way more zappy than magit
  (defun benj-quick-commit (arg)
    "Run git commit in projectile root with ARG as commit message"
    (interactive "sCommit Message: ")
    (benj-projectile-dir-command-to-string (format "git commit -m \"%s\"" arg)))


  (defun benj--git-diff-files (rev1 &optional rev2)
    "Get shell output for git diff files of REV1 agains REV2, if REV2 is ommitted, default to HEAD."
    (benj-projectile-dir-command-to-string (format "git diff --name-only %s..%s" rev1 (or rev2 "HEAD"))))

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

  (defun benj--get-diff-output-match-lines (regex rev1 &optional rev2)
    "Evaluate to a string containg the file names and lines matching REGEX
  in the git diff output of REV1 against REV2,
  if REV2 is ommitted it defaults to HEAD.
  this also sets the return value of `match-string'."
    ;; TODO line number would be cool
    (let ((ret)
          (files (benj--git-diff-files-list rev1 rev2)))
      (dolist (file files)
        (benj--log-to-diff-output (format "%s %d%%" file (/ (cl-position file files) (* (length files) 1.0)) 100))
        (let ((match (benj--file-diff-string-match file regex rev1 rev2)))
          (when match (setq ret (concat ret "\n" (format "File:%s : %s" file match) ret)))))
      (or ret (format "No line differences found for %s against %s" regex rev1 rev2)))

    )

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
    (benj--diff-output-match-lines "^+.*Debug.Log(.*).*$" "develop")))


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

(defun benj--git-diff-files-list (rev1 &optional rev2)
  "Get git diff files of REV1 against REV2, if REV2 is ommitted, default to HEAD."
  (split-string (benj--git-diff-files rev1 rev2)))


(defun benj-copy-last-yank-to-register (&optional reg)
  "Copy the contens of the \" register into REG.
Default is register a."
  (interactive)
  (evil-set-register (or reg ?a) (evil-get-register ?\" t)))


;; TODO fix case where there are no lines above us here
(defun benj-delete-some-whitespace ()
  "Delete a lot of white space but keep one line.
This is the same as vim `dipO'"
  (interactive)
  (re-search-backward "^.+$")
  (delete-blank-lines)
  (forward-line 1)
  (insert "\n"))

(defun benj-copy-file-pos-pretty ()
  "Copy the current pos in the format <filename> line <linenum>."
  (interactive)
  (kill-new
   (format "`%s` line %d"
           (file-name-base (buffer-file-name))
           (line-number-at-pos (point)))))

(defun benj-delete-til-closing-parem ()
  "Delete the rest until closing parem.
Basically evil `dt)'"
  (interactive)
  (let ((beg (point)))
    (search-forward ")")
    (forward-char -1)
    (delete-region beg (point))))

(defun benj-directory-files (path)
  "Get directory files from PATH. Excludes '.' and '..'."
  (let ((ret '()))
    (dolist (file (directory-files path t) ret)
      (unless (member (file-name-nondirectory file) '("." ".."))
        (setq ret (cons file ret))))
    ret))


(defun benj-clear-directory-contents (path)
  "Delete all files inside directory PATH."
  (dolist (file (benj-directory-files path))
    (delete-file file)))


;; temp
(defun benj-change-best-img ()
  (interactive)
  (search-forward-regexp "\\w+ Image \\(\\w+\\)")
  (message (match-string 1))
  (goto-char (line-end-position))
  (insert (format "\n     public BestImage best_%s" (match-string 1))))

(defun benj-mean (lst)
  (/ (float (apply '+ lst)) (length lst)))



(defconst benj-scratch-buffer-kinds
  '((:csharp . csharp-mode)
    (:fundamental . fundamental-mode)
    (:lisp-interaction . lisp-interaction-mode)
    (:markdown . markdown-mode)
    (:org . org-mode))
  "Map scratch buffer kind names with respective mode.
Form '(:key . MODE-FUNC)")

(defun benj--switch-to-scratch-buffer (arg)
  "Switch to one of the `'*scratch<name>*' buffers.
ARG should be one of `benj-scratch-buffer-kinds'"
  (let* ((buff-name (format "*scratch%s*" arg))
         (exists (get-buffer buff-name))
         (mode (cdr (assoc arg benj-scratch-buffer-kinds))))
    (switch-to-buffer (get-buffer-create buff-name))
    (when (and (not exists)
               (not (eq major-mode mode))
               (fboundp mode))
      (funcall mode))))





(defun benj-next-digit ()
  "Jump to next digit in buff"
  (interactive)
  (re-search-forward "[0-9]+")
  (forward-char -1))





;; TODO
(defun benj-rename-file-and-meta ()
  ;;
  )


(defun test-interactive (file-name)
  (interactive
   (let*
       ((default (thing-at-point 'evil-word))
        (str (read-string (format "New file name (default: %s)" default) nil nil default)))
     (list str)))
  (message file-name)
  )



(defun benj-remove-eol (file)
  "Remove crlf from FILE."
  (interactive"fRemove eol from file: ")
  (with-temp-file file
    (insert-file-contents-literally file)
    (while (re-search-forward "\r\n" nil t) (replace-match "\n"))))





;; todo needs a bit of work, only want the file names and nicer buffer
(defun benj-quick-file-usages ()
  "Search the project for the guid of the meta file you are visiting.
Or try to use the meta file of the file that you are visiting."
  (interactive)
  (if (buffer-file-name)
      (let* ((meta-file
              (if (string-equal (file-name-extension (buffer-file-name)) "meta")
                  (buffer-file-name)
               (concat (buffer-file-name) ".meta")))
             (guid
              (when meta-file
                (with-output-to-string
                 (with-temp-buffer meta-file
                                   (insert-file-contents-literally meta-file)
                                   (re-search-forward "guid: \\(\\w+\\)" nil t)
                                   (print (match-string 1)))))))
        (if guid
            ;;(start-process ) todo nicer buffer like that
            (let ((default-directory (projectile-project-root))
                  (command (format "rg --no-ignore  %s" (string-trim guid))))
              (message (format "run %s in %s" command default-directory))
              (async-shell-command command))
          (message "Could not get meta files guid.")))
    (message "not visiting a file.")))




















;; TEMP hack

;; original definition is in

(defun dir-locals-read-from-dir (dir)
  "Load all variables files in DIR and register a new class and instance.
DIR is the absolute name of a directory, which must contain at
least one dir-local file (which is a file holding variables to
apply).
Return the new class name, which is a symbol named DIR."
  (let* ((class-name (intern dir))
         (files (dir-locals--all-files dir))
	 ;; If there was a problem, use the values we could get but
	 ;; don't let the cache prevent future reads.
	 (latest 0) (success 0)
         (variables))
    (with-demoted-errors "Error reading dir-locals: %S"
      (dolist (file files)
	(let ((file-time (file-attribute-modification-time
			  (file-attributes file))))
	  (if (time-less-p latest file-time)
	    (setq latest file-time)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((newvars
                 (condition-case-unless-debug nil
                     ;; As a defensive measure, we do not allow
                     ;; circular data in the file/dir-local data.
                     (let ((read-circle nil))
                       (read (current-buffer)))
                   (end-of-file nil))))
            (setq variables
                  ;; Try and avoid loading `map' since that also loads cl-lib
                  ;; which then might hamper bytecomp warnings (bug#30635).
                  (if (not (and newvars variables))
                      (or newvars variables)
                    (require 'map)
                    (map-merge-with 'list (lambda (a b) (map-merge 'list a b))
                                    variables
                                    newvars))))))
      (setq success latest))
    (if (not (listp variables)) (setq variables '()))
    (setq variables (dir-locals--sort-variables variables))
    (dir-locals-set-class-variables class-name variables)
    (dir-locals-set-directory-class dir class-name success)
    class-name))
