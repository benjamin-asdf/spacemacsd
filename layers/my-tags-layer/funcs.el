(defvar cos-gtags-config-file "~/.tracked/.cos-gtags-config")
(defvar my-gtags-command "gtags --gtagslabel pygments --gtagsconf %s")


(defun my-regenerate-idlegame-tags-async ()
  "Regenerate cos gtags, assumes cos-gtags-config is set"
  (interactive)
  (let ((default-directory idlegame-project-root)
        (command (format my-gtags-command cos-gtags-config-file)))
    (message "regenerate tags command: %s" command)
    (async-shell-command command)))

;;ggtags-highlight-tag nil ;;maybe

(setq-default ggtags-auto-jump-to-match 'first)


(defun compilation-maybe-halt-auto-jump (buffer pos)
  "Halt jumping to first match in ggtags-global-mode if more that 1 results."
  (let* ((bname (buffer-name buffer))
         (ggtags (string-equal bname "*ggtags-global*")))
    (when ggtags
      (with-current-buffer buffer
        (let* ((lines (count-lines pos (point-max)))
               (halt (> lines 4))) ;; more than 4 seems to mean more than 1 match
          ;; (message (format "output lines %s halt? %s" lines halt))
          (when halt
            (setq compilation-auto-jump-to-first-error nil)))))))

(defun ggtags-query-tags (name)
  (interactive (list (ggtags-read-tag 'definition 1)))
  (ggtags-find-tag 'definition "--" (shell-quote-argument name)))

(advice-add 'compilation-auto-jump :before #'compilation-maybe-halt-auto-jump)
