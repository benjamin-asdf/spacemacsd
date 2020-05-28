
;;http://tuhdo.github.io/helm-intro.html
(with-eval-after-load 'helm
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )




(defconst benj-helm-leader-keys "oh")

(spacemacs/declare-prefix benj-helm-leader-keys "helm")

(progn
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (concat benj-helm-leader-keys (car x)) (cdr x)))
        '(("t" . helm-top)
          ("o" . helm-occur)
          ("i" . helm-info)
          ("d" . benj/helm-find-file-recursively)
          )))
