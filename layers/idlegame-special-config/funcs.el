(defvar idlegame-project-root "~/idlegame/IdleGame/")
(setq-default helm-candidate-number-limit 100)

;;projectile-fd
(defvar my-fd-command "fd -t f . -0")

(setq-default projectile-indexing-method 'alien)

(setq-default projectile-git-command my-fd-command)
(setq-default projectile-generic-command my-fd-command)

(setq projectile-enable-caching t)



;;grep, helm ag
(setq-default helm-ag-use-grep-ignore-list 't)

(with-eval-after-load 'grep
  (setq grep-find-ignored-files (append grep-find-ignored-files '("*.meta" "*.png" "*.unity" "*.tga" "*.psd" "*.anim" "*.prefab" "*.mat" "*.xls" "*.asset"))))

(with-eval-after-load 'grep
  (setq grep-find-ignored-directories (append grep-find-ignored-directories '("Library" "Packages" "Translations" "Design" "Sprites"))))

(with-eval-after-load 'helm
  (setq-default helm-ag-ignore-patterns '("*.meta" "*.unity" "*.js" "*.md" "*.exe" "*.prefab" "**/MessagePack/*" "**/Library/" "**/Design/" "*.xml" "*.asset" "**/Plugins/*")))
