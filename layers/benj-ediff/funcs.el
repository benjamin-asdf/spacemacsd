;; needs a bit of work, finishing session not nice yet

(defconst benj-ediff/merge-dir-name ".benj-merge-session")

(defvar benj-ediff/current-session
  "A plist of the form
:base file
:local file
:remote file
:merge file
:merger-pid number
:sess-dir dir"
  '())

(defun benj-ediff//put-current-sesson (prop val)
  "Put VAL in PROP for `benj-ediff/current-session'"
  (setq benj-ediff/current-session
        (plist-put benj-ediff/current-session :best "hello")))

(defun benj-ediff//set-current-session (sess-dir)
  "Look in SESS-DIR for merger filels and set `benj-ediff/current-session'."
  (let ((default-directory sess-dir))
    (setq benj-ediff/current-session '())
    (setq benj-ediff/current-session
          (plist-put benj-ediff/current-session :base  (benj-ediff//read-file "base")))
    (setq benj-ediff/current-session
          (plist-put benj-ediff/current-session :local (benj-ediff//read-file "local")))
    (setq benj-ediff/current-session
          (plist-put benj-ediff/current-session :remote (benj-ediff//read-file "remote")))
    (setq benj-ediff/current-session
          (plist-put benj-ediff/current-session :merged (benj-ediff//read-file "merged")))
    (setq benj-ediff/current-session
          (plist-put benj-ediff/current-session :merger-pid (benj-ediff//read-file "merger-pid")))
    (setq benj-ediff/current-session
          (plist-put benj-ediff/current-session :sess-dir sess-dir))))

(defun benj-ediff//read-file (file)
  "Return trimmed string, assumes that FILE contains a single line with a newline terminator"
  (string-trim (benj/read-file file)))

(defun benj-ediff/do-merge ()
  "Start current merge session in this git proj."
  (interactive)
  (when-let ((default-directory (magit-toplevel)))
    (unless (file-exists-p benj-ediff/merge-dir-name)
      (user-error "There is no benj merge session in %s" default-directory))
    (benj-ediff//set-current-session (expand-file-name benj-ediff/merge-dir-name))
    (benj-ediff//do-merge benj-ediff/current-session)))

(defun benj-ediff/resume-sess ()
  "Resume current session."
  (unless benj-ediff/current-session
    (user-error "No session active"))
  (benj-ediff//do-merge benj-ediff/current-session))

(defun benj-ediff//do-merge (sess)
  "Start or restart ediff session with SESS.
SESS should be a plist of the form described in `benj-ediff/current-session'."
  (ediff-merge-files-with-ancestor
   (plist-get sess :local)
   (plist-get sess :remote)
   (plist-get sess :base)
   nil
   (plist-get sess :merged)))

(defun benj-ediff/finish-session ()
  "Terminates the current session. Indicate that we are done merging."
  (interactive)
  (when-let ((pid (plist-get benj-ediff/current-session :merger-pid))
             (dir (plist-get benj-ediff/current-session :sess-dir)))
    (shell-command (format "kill %s" pid))
    (delete-directory dir t))
  (setq benj-ediff/current-session '()))















(defun ediff-copy-both-to-C ()
  "This is like combine with smerge."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "A" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
