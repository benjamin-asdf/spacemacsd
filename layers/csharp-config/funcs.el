(add-hook 'csharp-mode-hook 'ben-charp-hook)

(defun ben-charp-hook()
  (ben-change-csharp-style)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (define-key evil-normal-state-map "gh" 'omnisharp-current-type-information)
  (setq-local buffer-file-coding-system 'windows-1256-unix))

(defun ben-change-csharp-style()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4))
