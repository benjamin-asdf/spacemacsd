(defun mikus-reopen-buffer ()
  "Kill and open current BUFFER."
  (interactive)
  (let ( (buffer (buffer-name))
         (file (buffer-file-name))
         (point (point)) )
    (kill-buffer buffer)
    (find-file file)
    (goto-char point)))

(defun evil-find-WORD (forward)
  "Return WORD near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (evil-find-thing forward 'evil-WORD))

;; TODO it should copy the inner word, dunno
(defun copy-word-from-above ()
  "Copies the first found word from the line above."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line -1)
      (evil-goto-column col)
      (kill-new (concat (evil-find-WORD t) " "))))
  (yank))

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

(defun benj-best-message()
  "A random line chosen from best-message file."
  (let ((msgs (benj-read-lines best-messages-file)))
    (message (rand-element msgs))))

(defun rand-element (list)
  "Random element from LIST."
  (nth (random (length list)) list))
