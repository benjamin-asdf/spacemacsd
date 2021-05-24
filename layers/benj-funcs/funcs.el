;; funcs.el starts here;; funcs.el starts here
;;; Code:

(defun benj-best-message ()
  "A random line chosen from best-message file."
  (benj-rand-line-from-file best-messages-file))


(defun benj-delete-all-files (dir)
  "Delete all files inside DIR."
  (dolist (elem (directory-files dir))
    (unless (member elem '("." ".."))
      (delete-file (concat (file-name-as-directory dir) elem)))))

(defun benj-append-to-file (file content &optional newline)
  "Append a newline with CONTENT to FILE.
If NEWLINE is non nil, append a newline character."
  (unless (file-exists-p file)
    (write-region "" nil file))
  (with-temp-file file
    (insert-file-contents file)
    (goto-char (point-max))
    (insert (if newline (concat content "\n") content))))


(defun benj-process-other-window (process-name buffer-name process-program &rest process-args)
  "Start process and switch to output buffer in other window."
  (start-process process-name buffer-name process-program (mapconcat 'identity process-args " "))
  (unless (string-equal (buffer-name) buffer-name)
    (switch-to-buffer-other-window buffer-name)))



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


(defun benj-clear-directory-contents (path)
  "Delete all files inside directory PATH."
  (dolist (file (benj-directory-files path))
    (delete-file file)))



(defun benj--switch-to-scratch-buffer (mode)
  "Switch to one of the `'*scratch<name>*' buffers. MODE should be the name of a mode function."
  (let* ((buff-name (format "*scratch%s*" (mkstr mode)))
         (exists (get-buffer buff-name)))
    (switch-to-buffer (get-buffer-create buff-name))
    (when (and (not exists)
               (not (eq major-mode mode))
               (fboundp mode))
      (funcall mode))))


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



(defun benj/find-worktree-file-for-buff ()
  ;;   "This is because magit creates temp buffers when opening file history.
  ;; I didn't have a convinient way to visit the actual file when I'm in a buffer like that."
  "Try visit file without the ~ part of the buff name."
  (interactive)
  (let ((p (point)))
    (find-file (car (split-string (concat (magit-toplevel) (buffer-name)) ".~")))
    (goto-char p)
    (evil-scroll-line-to-center (line-number-at-pos))))


(defun benj/describe-last-function ()
  "Use `last-command' to bring up help of the last thing that happened."
  (interactive)
  (describe-function last-command))





(defun my/clear-konsoles ()
  (interactive)
  (async-shell-command "killall konsole"))



(defun benj-latest-screenshot ()
  "Get latest file located at \"~/Pictures/\" "
  (latest-file "~/Pictures"))

(defun benj/open-latest-screenshot ()
  (interactive)
  (spacemacs//open-in-external-app (benj-latest-screenshot)))

(defun benj/open-latest-vid ()
  (interactive)
  (spacemacs//open-in-external-app (latest-file "~/Videos/")))



(provide 'benj-funcs)
