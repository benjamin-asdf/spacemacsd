

















(define-minor-mode team/chsarp-superior-mode
  "This mode adds chsarp syntax to strings when you put them on multiple lines."
  :group 'electricity
  (if team/chsarp-superior-mode
      (progn
	      (add-hook 'post-self-insert-hook
                  #'team/csharp-superior-post-self-insert-function
        )
        )
    (remove-hook 'post-self-insert-hook #'team/csharp-superior-post-self-insert-function)
    ))

(defun team/csharp-superior-post-self-insert-function ()
  (when (and team/chsarp-superior-mode
             (looking-back
              (concat (regexp-opt '("if" "foreach" "using"))
                      "[[:blank:]]*([^;]+)[[:blank:]]*{"))
             (looking-at-p ".*;"))
    (let ((indent (current-indentation)))
      (kill-line)
      (team/->new-line)
      (yank)
      (team/->new-line)
      (indent-line-to indent)
      (insert "}")
      (forward-line -1)
      (evil-normal-state 1)
      (skip-chars-forward "[:blank:]"))))


(defun team/->new-line ()
  "Open new line and forward to there."
  (open-line 1)
  (forward-line 1))
