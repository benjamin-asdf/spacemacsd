(with-eval-after-load 'flyspell
  (add-hook 'text-mode-hook 'benj-spellcheck-text-mode-hook)
  (add-hook 'org-mode-hook 'benj-spellcheck-org-mode-hook))

(defun benj-spellcheck-text-mode-hook()
  (setq flyspell-mode t))

(defun benj-spellcheck-org-mode-hook()
  (setq flyspell-mode t))
