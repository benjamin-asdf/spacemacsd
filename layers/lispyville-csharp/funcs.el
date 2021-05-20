
(defun benj/enable-lispyville-chsarp ()
  (setq-local lispy-outline "^\\\\*+")
  (lispyville-mode)

  (define-key
    evil-normal-state-map
    (kbd "{")
    #'evil-backward-paragraph)
  (define-key
    evil-normal-state-map
    (kbd "}")
    #'evil-forward-paragraph)

  (define-key
    evil-normal-state-map
    (kbd "}")
    #'evil-forward-paragraph)

  (define-key
    evil-normal-state-map
    (kbd "L")
    #'evil-window-bottom)
  (define-key
    evil-normal-state-map
    (kbd "H")
    #'evil-window-top))



(defadvice lispyville-delete-line
    (after my/lispyville-delete-line-csharp-adv last acti)
  (when
      (eq major-mode 'csharp-mode)
    (save-excursion
      (if
          (and
           (looking-at ")$")
           (not
            (save-excursion
              (c-forward-token-1)
              (-->
               (char-after)
               (or (eq it ?.) (eq it ?{))))))
          (progn
            (forward-char 1)
            (insert ";")))
      (goto-char (point-at-eol))
      (when
          (eq (char-before) ?{)
        (forward-char -1)
        (insert " ")))))




(with-eval-after-load
    'csharp-mode
  (add-hook
   'csharp-mode-hook
   #'benj/enable-lispyville-chsarp))
