;;; Code:

(defconst best-messages-file "/home/benj/.homesick/repos/dotfiles/home/.config/my-messages")
(defconst banner-header "\n             Welcome to spacemacs.\n")

(defun best-banners--save-file-hook ()
  "Check if the current file is message file, maybe create banners.
If the file specified in best-banners-messages-file matches the saved file, we try to create banners"
  (when (and (equal buffer-file-name best-messages-file)
             (yes-or-no-p "Saved best-messages file, do you want to recreate spacemacs startup banners?"))
    (best-banners-recreate)))

(add-hook 'after-save-hook 'best-banners--save-file-hook)

(defun best-banners-recreate ()
  "Recreate banners from best-message file."
  (interactive)
  (unless (file-exists-p spacemacs-private-banner-directory)
      (make-directory spacemacs-private-banner-directory))
  (my-delete-all-files spacemacs-private-banner-directory)
  (dolist (msg (read-lines best-messages-file))
    (banners-create-new msg)))


(defun my-delete-all-files (dir)
  "Delete all files inside DIR."
  (dolist (elem (directory-files dir))
    (unless (member elem '("." ".."))
      (delete-file (concat (file-name-as-directory dir) elem)))))

(defun new-banner-file-name (&optional n)
  "New banner file name.
Optional N the file num to start from, meant for internal use"
  (let* ((num (if n n 0))
         (file (format "%s/%d.txt" spacemacs-private-banner-directory num)))
    (while (file-exists-p file)
      (setq file (new-banner-file-name (+ 1 num))))
    file))

(defun banners-create-new (msg)
  "Create a new banner with content MSG."
  (let ((banner (concat banner-header (shell-command-to-string (format "figlet \"%s\"" msg))))
        (banner-file (new-banner-file-name)))
    (write-region banner nil banner-file)))

(defun best-message()
  "A random line chosen from best-message file."
  (let ((msgs (read-lines best-messages-file)))
    (message (rand-element msgs))))

(defun rand-element (list)
  "Random element from LIST."
  (nth (random (length list)) list))


;; TODO s
;; random figlet fonts
;; open a buffer with the progress output

;;; funcs.el ends here
