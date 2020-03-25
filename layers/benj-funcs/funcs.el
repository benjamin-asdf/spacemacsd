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


(defun benj-new-shell-script (file)
  "Create a new script shell script with NAME in scripts dir."
  (interactive "FNew script: ")
  (unless (file-exists-p file)
    (write-region "#!/bin/sh\n\n" " " file))
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
    (let* ((command (if branch-name "git branch --show-current" "git rev-parse HEAD"))
            (output (benj-remove-newline-end-of-string
                    (benj-projectile-dir-command-to-string command))))
      (message output)
      (when auto-insert (insert output))
      (kill-new output)))

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
          (files (file (benj--git-diff-files-list rev1 rev2))))
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


(defun benj-windows-add-file-to-startup ()
  ;; Creates a copy of the current file in the default windows startup directory.
  ;; TODO warn user if not executable
  (interactive)
  (if buffer-file-name
      (copy-file buffer-file-name (format "~/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Startup/%s" (file-name-nondirectory buffer-file-name)))
    (message "Buffer is not visiting a file.")))


;; TODO
;; (defun benj-comment-out-unity-logs-in-buffer ()
;;   "Put csharp comment syntax before Debug\.Log."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward-regexp) "Debug\.Log")
;;     )

;;   )

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



(defun benj--diff-output-match-lines-debug (regex rev1 &optional rev2)
  "Open temp output buffer, show files and regex match of REGEX matching in the diff of REV1 against REV2
If REV2 is ommitted, default to HEAD."
  ;; (with-output-to-temp-buffer "*diff-match-lines*"
  ;;   (print (format "Git diff %s..%s, searching matching lines for %s ..." rev1 (or rev2 "HEAD") regex))
  ;;   (print (benj--get-diff-output-match-lines regex rev1 rev2))
  ;;   (print standard-output))
  (benj--log-to-diff-output (format "Git diff %s..%s, searching matching lines for %s ..." rev1 (or rev2 "HEAD") regex))
  (benj--log-to-diff-output (benj--get-diff-output-match-lines regex rev1 rev2)))
