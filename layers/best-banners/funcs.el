;;; Code:

(defconst best-messages-file "~/.homesick/repos/dotfiles/home/.config/my-messages")
(defconst banner-header "\n             Welcome to spacemacs.\n")

(defun best-banners--save-file-hook ()
  "Check if the current file is message file, maybe create banners.
If the file specified in best-banners-messages-file matches the saved file, we try to create banners"
  (when (and (equal buffer-file-name best-messages-file)
             (yes-or-no-p "Saved best-messages file, do you want to recreate spacemacs startup banners?"))
    (best-banners-recreate)))

;; (add-hook 'after-save-hook 'best-banners--save-file-hook)

(defun best-banners-recreate ()
  "Recreate banners from best-message file."
  (interactive)
  (unless (file-exists-p spacemacs-private-banner-directory)
      (make-directory spacemacs-private-banner-directory))
  (benj-delete-all-files spacemacs-private-banner-directory)
  (dolist (msg (benj-read-lines best-messages-file))
    (best-banners-create-new msg)))

(defun best-banners-next-banner-file (&optional n)
  "New banner file name.
Optional N the file num to start from, meant for internal use"
  (let* ((num (if n n 0))
         (file (format "%s/%d.txt" spacemacs-private-banner-directory num)))
    (while (file-exists-p file)
      (setq file (best-banners-next-banner-file (+ 1 num))))
    file))

(defun best-banners-create-new (msg)
  "Create a new banner with content MSG."
  (let ((banner (concat banner-header (shell-command-to-string (format "figlet \"%s\"" msg))))
        (banner-file (best-banners-next-banner-file)))
    (write-region banner nil banner-file)))




;; TODO s
;; random figlet fonts
;; open a buffer with the progress output

;;; funcs.el ends here
