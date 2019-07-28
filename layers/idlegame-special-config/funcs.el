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


;; unity-refresh
(defvar idlegame-lock-auto-refresh-file "/tmp/unity-auto-refresh.lock")
(defvar idlegame-user-refresh-file "/tmp/user-refresh-unity.file")

(defun idlegame-lock-auto-refresh ()
  "This sets the user auto refresh lock file for idlegame"
  (simple-touch-file idlegame-lock-auto-refresh-file))


(defun idlegame-refresh-unity ()
  "Creates the special user refresh file for some time"
  (interactive)
  (idlegame-touch-refresh-file))


(defun idlegame-touch-refresh-file ()
  (async-start
   (lambda ()
     ;; (simple-touch-file idlegame-user-refresh-file)
     (write-region "" nil idlegame-user-refresh-file)
     (sleep-for 4)
     t)

   (lambda (value)
     (if (file-exists-p idlegame-user-refresh-file)
         (delete-file idlegame-user-refresh-file)))))

(defun simple-touch-file (file)
  (write-region "" nil file))




;; TODO, better omnisharp startup.
;; (with-eval-after-load 'projectile
;;   (add-hook projectile-after-switch-project-hook 'my-projectile-switch-project-hook))


;; (defun my-projectile-switch-project-hook()
;;   (message "hello projectile switched project"))
