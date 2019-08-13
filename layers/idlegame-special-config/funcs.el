(defvar idlegame-project-root "~/idlegame/IdleGame/")



;;grep, helm ag
(setq-default helm-ag-use-grep-ignore-list 't)

(with-eval-after-load 'grep
  (setq grep-find-ignored-files (append grep-find-ignored-files '("*.meta" "*.png" "*.unity" "*.tga" "*.psd" "*.anim" "*.prefab" "*.mat" "*.xls" "*.asset"))))

(with-eval-after-load 'grep
  (setq grep-find-ignored-directories (append grep-find-ignored-directories '("Library" "Packages" "Translations" "Design" "Sprites"))))

(with-eval-after-load 'helm
  (setq-default helm-ag-ignore-patterns '("*.meta" "*.unity" "*.js" "*.md" "*.exe" "*.prefab" "**/MessagePack/*" "**/Library/" "**/Design/" "*.xml" "*.asset" "**/Plugins/*")))





;; TODO, better omnisharp startup.
;; (with-eval-after-load 'projectile
;;   (add-hook projectile-after-switch-project-hook 'my-projectile-switch-project-hook))


;; (defun my-projectile-switch-project-hook()
;;   (message "hello projectile switched project"))
