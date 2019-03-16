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
