
(define-derived-mode
  sxhkdrc-mode fundamental-mode
  "sxhkdrc"
  "Mode for displaying sxhkdrc"
  :syntax-table nil
  :abbrev-table nil
  (font-lock-add-keywords
   nil
   '(("^super " . 'font-lock-builtin-face)
     ("^#.*$" . 'font-lock-comment-face)
     ("+" . 'font-lock-constant-face)
     (";" . 'font-lock-constant-face)
     ("ctrl" . 'font-lock-keyword-face)
     ("+ \\(k\\) ;" 1 'font-lock-keyword-face)
     (#'(lambda (delim)
          (when (and
                 (looking-at-p "^super")
                 (> delim
                    (save-excursion
                      (forward-line 1)
                      (->$)
                      (point))))
            (forward-line 1)
            (re-search-forward ".*$")))
      'font-lock-function-name-face)
     )
   )
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(add-to-list 'auto-mode-alist
             '("sxhkdrc$" . sxhkdrc-mode))
