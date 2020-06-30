(defun benj-avy/take-word ()
  "Use `evil-avy-goto-subword-1', then yank the word at point."
  (interactive)
  (let ((word))
    (save-excursion
      (evil-avy-goto-subword-1)
      (setq word (thing-at-point 'evil-WORD)))
    (insert word)))


;; todo window bug

(defun benj-avy/take-word-part ()
  "See `benj-avy/take-word' but use the word part."
  (interactive)
  (let ((word)
        (window-point (window-point)))
    (save-excursion
      (evil-avy-goto-char-timer)
      (setq
       word
       (buffer-substring
        (point)
        (save-excursion
          (evil-forward-WORD-end)))))
    (insert word)
    (goto-char window-point)))




(defun benj-avy/take-region ()
  "Use `evil-avy-goto-char-timer' twice to get region to take to current postion."
  (interactive)
  (let ((beg)
        (end))
    (save-excursion
      (evil-avy-goto-char-timer)
      (setq beg (point)))
    (save-excursion
      (evil-avy-goto-char-timer)
      (setq end (+ (point) 1)))
    (insert (buffer-substring beg end))))
