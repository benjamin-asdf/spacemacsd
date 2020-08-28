
;;http://tuhdo.github.io/helm-intro.html
(with-eval-after-load 'helm
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )


(team/spacemacs-define-keys
 "oh"
 "helm"
  '("t" . helm-top)
  '("o" . helm-occur)
  '("i" . helm-info)
  '("d" . benj/helm-find-file-recursively)
  '("/" . (lambda () (interactive) (spacemacs/helm-project-smart-do-search t)))
  '("M" . helm-multi-swoop-current-mode))

(spacemacs/declare-prefix "ohs" "swwop")
(spacemacs/set-leader-keys
  "ohsb" 'team-helm/swoop-block-swoop
  "ohsf" 'team-helm/swoop-narrow-fun)


(team/spacemacs-define-keys
 "os"
 "search"
 '("c" . benj-helm-ag/comp-value-set)
 '("v" . benj-helm-ag/comp-value)
 '("f" . benj-helm-ag/flag-set)
 '("i" . benj-helm-ag/implementations)
 '("m" . benj-helm-ag/comp-matcher)
 '("M" . sailor-find-comp-matched)
 '("p" . benj-helm-ag/do-ag-prefixed)

 )


;; (spacemacs/set-leader-keys "ohqq" '#(lambda () (interactive)))
