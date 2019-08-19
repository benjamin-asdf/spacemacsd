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

(defun benj-best-message()
  "A random line chosen from best-message file."
  (let ((msgs (benj-read-lines best-messages-file)))
    (message (rand-element msgs))))

(defun rand-element (list)
  "Random element from LIST."
  (nth (random (length list)) list))
