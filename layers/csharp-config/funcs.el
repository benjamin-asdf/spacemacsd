
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

;; building my own now
;; (setq omnisharp-expected-server-version "1.35.2")


;; TEMP HACK
;; https://github.com/josteink/csharp-mode/issues/151
(defun csharp-disable-clear-string-fences (orig-fun &rest args)
  "This turns off `c-clear-string-fences' for `csharp-mode'. When
on for `csharp-mode' font lock breaks after an interpolated string
or terminating simple string."
  (unless (equal major-mode 'csharp-mode)
    (apply orig-fun args)))
(advice-add 'c-clear-string-fences :around 'csharp-disable-clear-string-fences)
