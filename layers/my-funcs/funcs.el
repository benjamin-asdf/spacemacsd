(defun my/evil-get-auto-substitute-pattern (useExPattern)
	"Try to get a substitute string automatically.
\  First look in evil-ex-search-pattern (only if USEEXPATTERN or in visual-mode)
\  Then look in 'evil-this-register'.
\  Lastly, use the 'word-at-point.'"
	(regexp-quote
	 (or (and evil-ex-search-pattern
						(or useExPattern (evil-visual-state-p) )
						(replace-regexp-in-string
						 "[\\<>]" "" (car evil-ex-search-pattern )))
			 (and evil-this-register (get-register evil-this-register))
			 (word-at-point))))

(defun my/evil-get-substitute-beg-end ()
	(if (evil-visual-state-p)
			"'<,'>" ""))

(defun my/make-group-pattern (pattern)
	(format "\\(%s\\)" pattern))

(defun my/evil-substitute (global pattern)
	"Start evil ex with some predefinded text for substitution.
\	 GLOBAL - says whether to use the global %s prefix
\	 PATTERN - string pattern to use"
	(let* ((command (format "%s%s/%s/"
												 (my/evil-get-substitute-beg-end)
												 (if global "%s" "s")
												 (my/make-group-pattern pattern)
												 )))
		(if pattern
				(evil-ex command)
			(message "pattern is nil"))))

(defmacro define-auto-substitute-command (command doc &optional global)
	"Defines quick substitute command.
\	 It will try to automatically get the pattern.
\  By default the substitute will be inline unless the GLOBAL is specified.
\  (fn COMMAND DOC GLOBAL)"
	(declare (indent defun)
					 (doc-string 2))
	(when command
		`(defun ,command ()
			 ,doc
			 (interactive)
			 (let* ((prefix-val (prefix-numeric-value current-prefix-arg))
							(use-ex-pattern (not (eq 1 prefix-val)) )
							(pattern (my/evil-get-auto-substitute-pattern use-ex-pattern)))
				 (my/evil-substitute ,global pattern)))))

(define-auto-substitute-command my/evil-global-substitute
	"Substitute globally."
	t)

(define-auto-substitute-command my/evil-line-substitute
	"Substitute inline."
	nil)



(defun reopen-buffer ()
  "Kill and open current BUFFER."
  (interactive)
  (let ( (buffer (buffer-name))
         (file (buffer-file-name))
         (point (point)) )
    (kill-buffer buffer)
    (find-file file)
    (goto-char point)))




;; Find files source
(with-eval-after-load 'helm-projectile
  (defun mikus-helm-projectile-find-file ()
    "Find file at point based on context."
    (interactive)
    (let* ((project-root (projectile-project-root))
          (project-files (projectile-current-project-files))
          (files (projectile-select-files project-files)))
      (if (= (length files) 1)
          (find-file (expand-file-name (car files) (projectile-project-root)))
        (helm :sources (helm-build-sync-source "Projectile files"
                        :candidates (if (> (length files) 1)
                                        (helm-projectile--files-display-real files project-root)
                                      (helm-projectile--files-display-real project-files project-root))
                        :fuzzy-match helm-projectile-fuzzy-match
                        :action-transformer 'helm-find-files-action-transformer
                        :keymap helm-projectile-find-file-map
                        :help-message helm-ff-help-message
                        :mode-line helm-read-file-name-mode-line-string
                        :action helm-projectile-file-actions
                        :persistent-action #'helm-projectile-file-persistent-action
                        :match-part (lambda (c) (helm-basename c))
                        :persistent-help "Preview file")
              :buffer "*helm projectile*"
              :truncate-lines helm-projectile-truncate-lines
              :prompt (projectile-prepend-project-name "Find file: ")))))

  (setq projectile-switch-project-action 'mikus-helm-projectile-find-file)
	)
