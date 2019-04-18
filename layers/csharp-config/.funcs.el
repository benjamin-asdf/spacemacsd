
(add-hook 'csharp-mode-hook 'ben-charp-hook)

(defun ben-charp-hook()
  (auto-complete-mode)
  (ben-change-csharp-style)
  (flycheck-mode)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (define-key evil-normal-state-map "gh" 'omnisharp-current-type-information)
  (define-key evil-insert-state-map (kbd "C-SPC") 'omnisharp-auto-complete)
  (setq-local eldoc-idle-delay 0.8)
  (setq fringe-mode 'no-fringes))

(defun ben-change-csharp-style()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4))


