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
      (shell-command-to-string command))))

(defun benj-remove-newline-end-of-string (string)
  "Remove newline characters at the end of STRING."
  (replace-regexp-in-string "\n\\'" "" string))


(defun benj-copy-last-yank-to-register (&optional reg)
  "Copy the contens of the \" register into REG.
Default is register a."
  (interactive)
  (evil-set-register (or reg ?a) (evil-get-register ?\" t)))


(defun benj-delete-some-whitespace ()
  "Delete a lot of white space but keep one line.
This is the same as vim `dipO'"
  (interactive)
  (re-search-backward "^.+$")
  (delete-blank-lines)
  (forward-line 1)
  (insert "\n"))



;; TODO
;; (defun benj-comment-out-unity-logs-in-buffer ()
;;   "Put csharp comment syntax before Debug\.Log."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward-regexp) "Debug\.Log")
;;     )

;;   )
