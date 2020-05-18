
(add-hook 'csharp-mode-hook 'benj-charp-hook)

(defun benj-charp-hook()
  (benj-change-csharp-style)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (define-key evil-normal-state-map "gh" 'omnisharp-current-type-information)
  ;; TODO needs a bit of work
  (define-key evil-normal-state-map "ge" (lambda () (interactive)
                                           (flycheck-cancel-error-display-error-at-point-timer)
                                           (flycheck-display-error-at-point)))
  ;; (electric-pair-mode -1)
  (smartparens-strict-mode -1)
  (setq-local buffer-file-coding-system 'windows-1256-unix))

(defun benj-change-csharp-style()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(setq omnisharp-expected-server-version "1.35.0")
