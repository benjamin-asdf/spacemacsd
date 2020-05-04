



(defun team-helm-projectile-find-many-files ()
  "Simple implementation of projectile find file without any file filters"
  (interactive)
  (team-helm-projectile--find-files "fd -H -E=.git -I -tf -0 ."))

(defun team-helm-projectile--find-files (file-cmd)
  "Subroutine for starting a simple helm projectile search
FILE-CMD is used for collecting the is the command used for collecting files.
It should expect to be run with `projectile-project-root' and return a null character seperated list of files on the disk."
  (let* ((project-root (projectile-ensure-project (projectile-project-root)))
          (files (projectile-files-via-ext-command project-root file-cmd))
          (file (projectile-completing-read "Find file: "
                                            files)))
    (when file
      (progn (funcall #'find-file (expand-file-name file project-root))
              (run-hooks 'projectile-find-file-hook)))))


;;WIP
(defun team-helm-projectile-fd-find (dir &optional ext &optional regex)
  "Use `team-helm-projectile--find-files' as subroutine.
Build an fd command with DIR and REGEX. DIR should either be expanded, or relative to `projectile-project-root'."
  (team-helm-projectile--find-files (format "fd -I -E=.git")))

(defun team-idlegame-find-prefabs ()
  "Uses `team-helm-projectile--find-files'.
Search Asset dir for .prefabs"
  (interactive)
  (let ((default-directory projectile-project-root))
    (team-helm-projectile--find-files "fd -tf -I -0 -e prefab . IdleGame/Assets/")))

(defun team-helm-projectile-initial-regex (regex)
  "Start helm projectile but collect files with a predefined regex REGEX."
  (interactive"sInitial regex: "))


;; (lambda () (interactive) (benj-helm-projectile-initial-regex "\\.prefab$"))
