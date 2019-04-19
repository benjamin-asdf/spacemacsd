(defvar cos-gtags-config-file "~/.tracked/.cos-gtags-config")
(defvar my-gtags-command "gtags --gtagslabel pygments --gtagsconf %s")
(defvar idlegame-project-root "~/idlegame/IdleGame/")


(defun my-regenerate-idlegame-tags-async ()
  "Regenerate cos gtags, assumes cos-gtags-config is set"
  (interactive)
  (let ((default-directory idlegame-project-root)
        (command (format my-gtags-command cos-gtags-config-file)))
    (message "regenerate tags command: %s" command)
    (async-shell-command command)))
