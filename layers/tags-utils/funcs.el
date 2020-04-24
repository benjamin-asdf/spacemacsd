(with-eval-after-load 'helm-projectile
    (progn

        (defvar idle-game-best-folders '( "Assets/#/Sources" "Assets/#/Scripts" "Assets/Editor" ))
        (with-eval-after-load 'grep
          (defvar idle-game-ignored-files (append grep-find-ignored-files '("*.asset" "*.java" "*.m" "MessagePack")))
          )

        (defvar projectile-custom-ignored-files '())

        (defun tags-custom-ignored-files ()
            (if (string-equal ( projectile-project-root ) idlegame-project-root)
                    idle-game-ignored-files
                projectile-custom-ignored-files))

        (defun idle-game-folders ()
            (mapconcat (lambda (path) (format "\"%s\"" (concat idlegame-project-root path))) idle-game-best-folders " "))

        (defun regenerate-idlegame-tags ()
            (interactive)
            (let* ((dirs (idle-game-folders))
                         (projectile-tags-command "ctags -Re -f \"%s\" %s %s"))
                (projectile-regenerate-tags-async dirs)))
        
        (defun projectile-regenerate-tags-for-cos-generated-files ()
            "Regenerate tags for this file and append it to the project's TAGS file."
            (interactive)
            (let* ((dir (concat idlegame-project-root "Assets/#/Sources/Generated"))
                         (projectile-tags-command "ctags -Re -f \"%s\" %s -a \"%s\""))
                (projectile-regenerate-tags-async dir)))
        
        (defun projectile-regenerate-tags-for-current-file-async ()
            "Regenerate tags for this file."
            (interactive)
            (let* ((current-file (buffer-file-name))
                         (projectile-tags-command "ctags -Re -f \"%s\" %s -a \"%s\""))
                (projectile-regenerate-tags-async current-file)))

        (defun projectile-tags-exclude-patterns ()
            "Return a string with exclude patterns for ctags."
            (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
                                                                                     (directory-file-name pattern)))
                                 (append (tags-custom-ignored-files) (projectile-ignored-directories-rel)) " "))

        (defun regenerate-tags ()
            (interactive)
            (if (string-equal ( projectile-project-root ) idlegame-project-root)
                    (regenerate-idlegame-tags)
                (projectile-regenerate-tags-async)))

        (defun projectile-regenerate-tags-async (&optional files append)
            "Regenerate the project's [e|g]tags.  Optionally specify FILES."
            (interactive)
            (let* ((project-root (projectile-project-root))
                         (tags-exclude (projectile-tags-exclude-patterns))
                         (default-directory project-root)
                         (tags-file (expand-file-name projectile-tags-file-name))
                         (command (format projectile-tags-command tags-file tags-exclude (or files default-directory))))
                (message "regenerate tags command: %s" command)
                (async-shell-command command)))))
