
(add-hook 'csharp-mode-hook 'benj-charp-hook)

(defun benj-charp-hook()
  (benj-change-csharp-style)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (define-key evil-normal-state-map "gh" 'omnisharp-current-type-information)
  ;; TODO needs a bit of work
  (define-key evil-normal-state-map "ge" (lambda () (interactive)
                                           (flycheck-cancel-error-display-error-at-point-timer)
                                           (flycheck-display-error-at-point)))
  (smartparens-strict-mode -1)
  ;; TODO
  (when (> (line-number-at-pos (point-max)) 500)
    (jit-lock-mode nil)))


;; fancy shvancy diable ggtags eldoc func
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


(defun benj-change-csharp-style()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4))



(add-to-list 'auto-mode-alist '("\\.ruleset\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.props\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.targets\\'" . xml-mode))



(defun benj-omnisharp/unit-test-advice (&rest args)
  (save-some-buffers))
(advice-add 'omnisharp-unit-test-at-point :before #'benj-omnisharp/unit-test-advice)
(advice-add 'omnisharp-unit-test-buffer :before #'benj-omnisharp/unit-test-advice)
(advice-add 'omnisharp-unit-test-last :before #'benj-omnisharp/unit-test-advice)





;; building my own now
;; use `benj/omnisharp-start-near-proj'
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
