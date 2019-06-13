
(defun reopen-buffer ()
  "Kill and open current BUFFER."
  (interactive)
  (let ( (buffer (buffer-name))
         (file (buffer-file-name))
         (point (point)) )
    (kill-buffer buffer)
    (find-file file)
    (goto-char point)))




;; Find files source
(with-eval-after-load 'helm-projectile
  (defun mikus-helm-projectile-find-file ()
    "Find file at point based on context."
    (interactive)
    (let* ((project-root (projectile-project-root))
          (project-files (projectile-current-project-files))
          (files (projectile-select-files project-files)))
      (if (= (length files) 1)
          (find-file (expand-file-name (car files) (projectile-project-root)))
        (helm :sources (helm-build-sync-source "Projectile files"
                        :candidates (if (> (length files) 1)
                                        (helm-projectile--files-display-real files project-root)
                                      (helm-projectile--files-display-real project-files project-root))
                        :fuzzy-match helm-projectile-fuzzy-match
                        :action-transformer 'helm-find-files-action-transformer
                        :keymap helm-projectile-find-file-map
                        :help-message helm-ff-help-message
                        :mode-line helm-read-file-name-mode-line-string
                        :action helm-projectile-file-actions
                        :persistent-action #'helm-projectile-file-persistent-action
                        :match-part (lambda (c) (helm-basename c))
                        :persistent-help "Preview file")
              :buffer "*helm projectile*"
              :truncate-lines helm-projectile-truncate-lines
              :prompt (projectile-prepend-project-name "Find file: ")))))

  ;;TODO fix bug.
  ;; (setq projectile-switch-project-action 'mikus-helm-projectile-find-file)
	)


(defun evil-find-WORD (forward)
  "Return WORD near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (evil-find-thing forward 'evil-WORD))

(defun copy-word-from-above ()
  "Copies the first found word from the line above."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line -1)
      (evil-goto-column col)
      (kill-new (concat (evil-find-WORD t) " "))))
  (yank))
