

(define-minor-mode team/chsarp-superior-mode
  "This mode adds chsarp syntax to strings when you put them on multiple lines."
  :group 'electricity
  (if team/chsarp-superior-mode
      (progn
	      (add-hook 'post-self-insert-hook
                  #'team/csharp-superior-post-self-insert-function
        )


        ;; (font-lock-add-keywords
        ;;  nil
        ;;  '(("\\bMatcher\\b" . speedbar-file-face))

        ;;  )

        )
    (remove-hook 'post-self-insert-hook #'team/csharp-superior-post-self-insert-function)))

(add-hook 'csharp-mode-hook #'team/chsarp-superior-mode)

(defun my-matcher-func (arg)
  (re-search-forward "\\(Matcher\\)\\." arg t)
  )

(defun team/csharp-superior-post-self-insert-function ()
  (when team/chsarp-superior-mode
    (or
     (team/catch-comp-on-line)
     ;; brackets
     (and
      (looking-back "{" (point-at-bol))
      (looking-at "\\([[:blank:]]*)[[:blank:]]*\\(;\\)?\\)\\|\\(.*\"\\)\\|\\([[:blank:]]*.+\\)")
      (let ((indent (current-indentation)))
        (unless (match-string 3)
          (if (match-string 1)
             (progn
               (kill-line)
               (team/->new-line)
               (insert "}")
               (yank)
               (unless (looking-back ";")
                 (insert ";"))
               (indent-line-to indent)
               (forward-line -1)
               (goto-char (point-at-eol))
               (team/->new-line)
               (indent-line-to (+ 4 indent)))
           (kill-line)
           (team/->new-line)
           (yank)
           (team/->new-line)
           (indent-line-to indent)
           (insert "}")
           (forward-line -1)
           (evil-normal-state 1)))
        (skip-chars-forward "[:blank:]")))

     ;; strings across lines
     (and
      (looking-back "\n")
      (save-excursion
        (forward-line -1)
        (re-search-forward "\\(.+?\".+?\"\\)\\|\\(.+?\"\\)" (point-at-eol) t))
      (match-string 2)
      (let ((indent (current-indentation)))
        (forward-line -1)
        (line->$)
        (insert "\" +")
        (forward-line 1)
        (insert "\"")
        (indent-line-to indent))))))


(defun team/->new-line ()
  "Open new line and forward to there."
  (open-line 1)
  (forward-line 1))

(defun line->$ ()
  "Goto end of line."
  (goto-char (point-at-eol)))

(defun team/catch-comp-on-line ()
  "Try search for comp syntax on current line,
if successfull, set to register m and return non nil.
Nil otherwise."
  (interactive)
  (prog1 (team/re-this-line
          "public class \\(\\w+\\) : \\w+Component.*{ }" t)
    (evil-set-register ?m (match-string-no-properties 1))))
