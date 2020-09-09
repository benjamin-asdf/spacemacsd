(defconst cos-dir (or (getenv "COSDIR") "~/clashofstreamers"))
(defconst idlegame-project-root (concat (file-name-as-directory cos-dir) "IdleGame/"))
(defconst idlegame-assets-dir (concat (file-name-directory idlegame-project-root) "Assets/"))

;;grep, helm ag
(setq-default helm-ag-use-grep-ignore-list 't)

(with-eval-after-load 'grep
  (setq grep-find-ignored-files (append grep-find-ignored-files '("*.meta" "*.png" "*.unity" "*.tga" "*.psd" "*.anim" "*.prefab" "*.mat" "*.xls" "*.asset"))))

(with-eval-after-load 'grep
  (setq grep-find-ignored-directories (append grep-find-ignored-directories '("Library" "Packages" "Translations" "Design" "Sprites"))))

(with-eval-after-load 'helm
  (setq-default helm-ag-ignore-patterns '("*.meta" "*.unity" "*.js" "*.md" "*.exe" "*.prefab" "*.xml" "*.asset" "**/Plugins/*")))



;; TEMP FIXME
(with-eval-after-load 'projectile
  (setq projectile-git-submodule-command nil))


;; need to load csharp layer first
(defun benj-open-idlegame-project ()
  "Open idlegame solution file and start omnisharp."
  (interactive)
  (find-file-other-window (concat (file-name-as-directory idlegame-project-root) "IdleGame.sln"))
  (omnisharp--do-server-start idlegame-project-root))


;; TODO, better omnisharp startup.
;; (with-eval-after-load 'projectile
;;   (add-hook projectile-after-switch-project-hook 'my-projectile-switch-project-hook))


;; (defun my-projectile-switch-project-hook()
;;   (message "hello projectile switched project"))

(provide 'idlegame-definitions)
