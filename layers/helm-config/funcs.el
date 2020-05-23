
;; rg
(setq-default helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
(setq-default helm-ag-use-grep-ignore-list 't)

(setq-default helm-candidate-number-limit 100)



(setq-default helm-ag-base-command "rg --color=never --no-heading" )


;; (rg-enable-menu)
