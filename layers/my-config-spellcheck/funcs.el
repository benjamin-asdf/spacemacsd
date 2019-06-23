(with-eval-after-load 'flyspell
  (add-hook 'text-mode-hook 'benj-text-mode-hook)
  (add-hook 'org-mode-hook 'benj-org-mode-hook))

(defun benj-text-mode-hook()
  (setq flyspell-mode t))

(defun benj-org-mode-hook()
  (setq flyspell-mode t))
