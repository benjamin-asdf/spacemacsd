
(add-hook 'csharp-mode-hook 'benj-csharp-hook)

(defun benj-csharp-hook()

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (define-key evil-normal-state-map "gh" 'omnisharp-current-type-information)
  (flycheck-cancel-error-display-error-at-point-timer)
  (smartparens-strict-mode -1)



  (setq-local helm-swoop-speed-or-color (< (line-beginning-position (point-max)) 800))

  (when (> (line-number-at-pos (point-max)) 500)
    (jit-lock-mode nil)))


(setq omnisharp-auto-complete-want-documentation nil)

;; too slow in big files.
;; (setq omnisharp-imenu-support t)

(defgroup ggtags nil
	"Options for ggtags"
	:group 'init)

(defcustom ggtags-eldoc-disabled-major-modes '()
	"List of major modes in which ggtags-eldoc should not run"
	:group 'ggtags
	:type '(set symbol))

(setq ggtags-eldoc-disabled-major-modes '(csharp-mode))

(defun benj/ggtags-eldoc-advice (original-func &rest args)
  "Advice around `ggtags-eldoc-function', do nothing if we are in one of `ggtags-eldoc-disabled-major-modes'."
  (unless (memq major-mode ggtags-eldoc-disabled-major-modes)
    (apply original-func args)))

(advice-add 'ggtags-eldoc-function :around #'benj/ggtags-eldoc-advice)


(add-to-list 'auto-mode-alist '("\\.ruleset\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.props\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.targets\\'" . xml-mode))



(defun benj-omnisharp/unit-test-advice (&rest args)
  (save-some-buffers))
(advice-add 'omnisharp-unit-test-at-point :before #'benj-omnisharp/unit-test-advice)
(advice-add 'omnisharp-unit-test-buffer :before #'benj-omnisharp/unit-test-advice)
(advice-add 'omnisharp-unit-test-last :before #'benj-omnisharp/unit-test-advice)

