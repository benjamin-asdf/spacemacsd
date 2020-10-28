(defun my/toggle-parens-display (&optional explicit)
  (interactive)
  (setq
   show-paren-style
   (or
    explicit
    (cadr (memql show-paren-style '(parenthesis expression mixed)))
    'parenthesis))
  (if (eq show-paren-style 'parenthesis)
      (set-face-underline 'show-paren-match "White")
    (set-face-underline 'show-paren-match nil))
  (message "%s is now %s"
           (mkstr 'show-paren-style)
           (mkstr show-paren-style)))

;; FIXME make it work, crashes atm around line delimeters
(with-eval-after-load 'smartparens
  (show-smartparens-global-mode -1)
  (show-smartparens-mode -1)

  ;; sticking to show paren mode instead
  (set-face-foreground 'show-paren-match "White")
  (my/toggle-parens-display 'parenthesis)


  (show-paren-mode 1))
