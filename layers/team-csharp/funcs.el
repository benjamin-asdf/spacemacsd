

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
     ;; brackets
     (and
      (looking-back "{" (point-at-bol))
      (looking-at "\\([[:blank:]]*)[[:blank:]]*\\(;\\)?\\)\\|\\([[:blank:]]*.+\\)")
      (let ((indent (current-indentation)))
        (if (match-string 1)
            (progn
              (kill-line)
              (team/->new-line)
              (insert "});")
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
          (evil-normal-state 1))
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
