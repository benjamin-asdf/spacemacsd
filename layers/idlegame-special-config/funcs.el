(defconst cos-dir (or (getenv "COSDIR") "~/clashofstreamers"))
(defconst idlegame-project-root (concat (file-name-as-directory cos-dir) "IdleGame/"))
(defconst idlegame-assets-dir (concat (file-name-directory idlegame-project-root) "Assets/"))
(defconst idlegame-sources-dir (concat (file-name-directory idlegame-assets-dir) "#/Sources/"))
(defconst idlegame-loadgroups-dir (concat (file-name-directory idlegame-assets-dir) "LoadGroups/"))

;;grep, helm ag
(setq-default helm-ag-use-grep-ignore-list 't)

(with-eval-after-load 'grep
  (setq grep-find-ignored-files (append grep-find-ignored-files '("*.meta" "*.png" "*.unity" "*.tga" "*.psd" "*.anim" "*.prefab" "*.mat" "*.xls" "*.asset"))))

(with-eval-after-load 'grep
  (setq grep-find-ignored-directories (append grep-find-ignored-directories '("Library" "Packages" "Translations" "Design" "Sprites"))))

(with-eval-after-load 'helm
  (setq-default helm-ag-ignore-patterns '("*.meta" "*.unity" "*.js" "*.md" "*.exe" "*.prefab" "*.xml" "*.asset" "**/Plugins/*")))


(defmacro cos/with-root (&rest body)
  (declare (debug t))
  `(team/with-default-dir
    ,cos-dir
    ,@body))

(defmacro cos/with-idlegame-proj-root (&rest body)
  (declare (debug t))
  `(team/with-default-dir
    ,idlegame-project-root
    ,@body))


;; TEMP FIXME
(with-eval-after-load 'projectile
  (setq projectile-git-submodule-command nil))


(defun benj-open-idlegame-project ()
  "Open idlegame solution file and start omnisharp."
  (interactive)
  (with-temp-buffer
    (csharp-mode))
  (find-file-other-window (concat (file-name-as-directory idlegame-project-root) "IdleGame.sln"))
  (omnisharp--do-server-start idlegame-project-root))

(defun team/insert-idlegame-dir ()
  (interactive)
  (insert  idlegame-project-root))


(defun team/cut-path-to-idlegame ()
  (interactive)
  (save-excursion
    (->0)
    (when (re-search-forward (format "\\(.*\\)\\(%s/\\)\\(.*\\)" cos-dir nil) (point-at-eol) t)
      (replace-match (format "%s%s" (match-string 1) (match-string 3))))))


(defun my/cos-jump-asset-file ()
  "If there is an Assets/ filename at point, jump to it in the cos project."
  (interactive)
  (unless
      (-some-->
          (thing-at-point 'filename)
        (and (s-prefix-p "Assets/" it) it)
        (team/with-default-dir
         idlegame-project-root
         (find-file-at-point it)
         t))
    (user-error "Thing at point is not an Assets/ file name.")))


(defun cos/cs-fiels-with-matches (re)
  "See `files-with-matches'."
  (team/with-default-dir
   idlegame-assets-dir
   (--filter
    (string-match-p "\.cs$" it)
    (files-with-matches re))))



(provide 'idlegame-definitions)
