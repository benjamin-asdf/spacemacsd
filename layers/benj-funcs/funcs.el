;; funcs.el starts here;; funcs.el starts here
;;; Code:
(defun mikus-reopen-buffer ()
  "Kill and open current BUFFER."
  (interactive)
  (let ( (buffer (buffer-name))
         (file (buffer-file-name))
         (point (point)) )
    (kill-buffer buffer)
    (find-file file)
    (goto-char point)))

;; some of these are literally the first lisp I ever put,
;; take these as examples of bad/ beginner style
;; benj--word-in-column should not exist. In elisp, the paradign is to move around in the buffer
;; use funcs like `forward-line', `skip-chars-backward', `exchange-point-and-mark'

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
  "Evil word in LINE and COL.
single space if char at point is a space. Nil for empty lines."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
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

(defun benj-copy-last-yank-to-register (&optional reg)
  "Copy the contens of the \" register into REG.
Default is register a."
  (interactive)
  (evil-set-register (or reg ?a) (evil-get-register ?\" t)))

(defun team/pull-register-2-to-b ()
  "Not documented."
  (interactive)
  (evil-set-register ?b (evil-get-register ?2 t)))


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

;; NOTE that this is redundant `directory-files' already provides all the functionality
(defun benj-directory-files (path &optional pattern)
  "Get directory files from PATH. Excludes '.' and '..'.
Match file names for a PATTERN, if non nil."
  (let ((ret '()))
    (dolist (file (directory-files path t) ret)
      (when (and (not (member (file-name-nondirectory file) '("." "..")))
                 (or (not pattern) (string-match-p pattern (file-name-nondirectory file))))
        (setq ret (cons file ret))))
    ret))


(defun benj-clear-directory-contents (path)
  "Delete all files inside directory PATH."
  (dolist (file (benj-directory-files path))
    (delete-file file)))





;; TODO add here and extension
(defconst benj-scratches/buffer-kinds
  '((:csharp . csharp-mode)
    (:fundamental . fundamental-mode)
    (:lisp-interaction . lisp-interaction-mode)
    (:markdown . markdown-mode)
    (:org . org-mode))
  "Map scratch buffer kind names with respective mode.
Form '(:key . MODE-FUNC)")

(defconst benj-scratches/scratches-dir "~/.local/scratches/")

(defun benj-scratches/file-name (mode)
  "Get file name for todays scratch for MODE."
  (format "%s%s%s" benj-scratches/scratches-dir (format-time-string "%Y-%m-%d/") mode))

;; TODO functionality to open a second scratch for the day
(defun benj--switch-to-scratch-buffer (arg)
  "Switch to one of the `'*scratch<name>*' buffers.
ARG should be one of `benj-scratches/buffer-kinds'.
There is a scratch file for each day and mode."
  (let* ((buff-name (format "*scratch%s*" arg))
         (exists (get-buffer buff-name))
         (mode (cdr (assoc arg benj-scratches/buffer-kinds))))
    (switch-to-buffer (get-buffer-create buff-name))
    (when (and (not exists)
               (not (eq major-mode mode))
               (fboundp mode))
      (funcall mode))))


;; create dir

;; (defun benj--switch-to-scratch-buffer (arg)
;;   "Switch to one of the `'*scratch<name>*' buffers.
;; ARG should be one of `benj-scratches/buffer-kinds'.
;; There is a scratch file for each day and mode."
;;   (find-file (benj-scratches/file-name (assoc-default arg benj-scratches/buffer-kinds)))
;;   (when (and (not (eq major-mode mode))
;;              (fboundp mode))
;;     (funcall mode)))



(defun benj-next-digit ()
  "Jump to next digit in buff."
  (interactive)
  (re-search-forward "[0-9]+")
  (forward-char -1))

(defun test-interactive (file-name)
  (interactive
   (let*
       ((default (thing-at-point 'evil-word))
        (str (read-string (format "New file name (default: %s)" default) nil nil default)))
     (list str)))
  (message file-name))

(defun benj-remove-eol (file)
  "Remove crlf from FILE."
  (interactive"fRemove eol from file: ")
  (benj-remove-eol-from-file file))

(defun benj/remove-eol-from-buff ()
  "Remove crlf from buff."
  (interactive)
  (while (re-search-forward "\r\n" nil t) (replace-match "\n")))

(defun benj-remove-eol-from-file (&optional file)
  "Remve crlf from FILE. Try buffer file if FILE is nil."
  (interactive)
  (setq file (or file buffer-file-name))
  (with-temp-file file
    (insert-file-contents-literally file)
    (benj/remove-eol-from-buff)))

;; see `magit-unmerged-files', stuff already exists
(defun benj-unmerged-prefabs ()
  "List currently unmerged prefabs"
  (let ((default-directory (magit-toplevel)))
    (seq-filter (lambda (s) (string-match-p "prefab$" s)) (magit-unmerged-files))))

(defun benj-write-prefabs-for-rewrite (&optional prefabs)
  "Put PREFABS into a file for unity script processing.
If PREFABS is ommitted or nil, take the currently unmerged."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (write-region (mapconcat (lambda (s) (string-trim-left s "IdleGame/")) (or prefabs (benj-unmerged-prefabs)) "\n") nil "IdleGame/prefabs-for-rewrite.txt")))


(defun benj-all-changed-files (rev1 rev2 regex)
  "List all changed files between REV1 and REV2 that match REGEX
REV1 defaults to develop, if nil, REV2 defaults to HEAD, if nil."
  (seq-filter
   (or (not regex) (lambda (s) (string-match-p regex s)))
   (magit-changed-files (or rev1 "develop") rev2)))


(defun benj-changed-prefabs (rev1 rev2)
  "Uses `benj-all-changed-files' as subroutine.
Also see `magit-changed-files'
Default to develop and HEAD."
  (benj-all-changed-files rev1 rev2 "\\.prefab$"))

(defun benj-checkout-develop-prefabs ()
  "Checkout all changed prefabs from develop."
  (interactive)
  (benj-checkout-files-from-develop "\\.prefab$"))

(defun benj-checkout-files-from-develop (regex)
  "Checkout changed files matching REGEX from develop."
  (let ((default-directory (magit-toplevel)))
    (async-shell-command
     (concat "git checkout develop -- " (combine-and-quote-strings (benj-all-changed-files "develop" "HEAD" regex))))))





(defun benj-git-repo-root ()
  "Current git repo root.
Depends on `default-directory'"
  (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))


(defun mikus-clist (new-el &rest args)
	(let ((res (or (and (listp new-el) new-el) (list new-el))))
		(mapc (lambda (x)
            (setq res
                  (or (and (listp x) (append res x))
                      (cons x res))))
          args)
    res))





(defun benj-proc-with-buff (name buffname program &rest args)
  "See `start-process', also pop to the buffer.
NAME and BUFFNAME are allowed to be nil."
  (let ((name (or name "benj-proc"))
        (buffname (or buffname "*benj-proc*"))
        )
    (start-process name buffname program args)
    (pop-to-buffer buffname)))


(defun benj/find-worktree-file-for-buff ()
  ;;   "This is because magit creates temp buffers when opening file history.
  ;; I didn't have a convinient way to visit the actual file when I'm in a buffer like that."
  "Try visit file without the ~ part of the buff name."
  (interactive)
  (let ((p (point)))
    (find-file (car (split-string (concat (magit-toplevel) (buffer-name)) ".~")))
    (goto-char p)
    (evil-scroll-line-to-center)))


(defun benj/describe-last-function ()
  "Use `last-command' to bring up help of the last thing that happened."
  (interactive)
  (describe-function last-command))


(defun benj/read-file (file)
  "Read all contents of FILE."
  (with-temp-file file
    (insert-file-contents-literally file)
    (princ (buffer-substring-no-properties (point-min) (point-max)))))

(defun benj-text/add-crlf (&optional file)
  "Add crlf to FILE."
  (interactive"fFile to add crlf")
  (with-temp-file
      file
    (insert-file-contents-literally file)
    (while (re-search-forward "\n" nil t)
      (replace-match "\r\n"))))

(defun benj-text/add-crlf-this-file ()
  "Add crlf to current file"
  (interactive)
  (if (buffer-file-name)
      (benj-text/add-crlf (buffer-file-name))
    (user-error "Buffer is not visiting a file.")))







































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
