;; FIXME make it work, crashes atm around line delimeters
(with-eval-after-load 'smartparens
  (show-smartparens-global-mode -1)
  (show-smartparens-mode -1)

  ;; sticking to show paren mode instead
  (set-face-foreground 'show-paren-match "White")
  (set-face-underline 'show-paren-match "White")

  (show-paren-mode 1))
