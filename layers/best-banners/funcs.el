(defconst best-messages-file "~/.config/my-messages")
(defconst banner-header "\n             Welcome to spacemacs.\n")




(defun best-banners--save-file-hook ()
  "This hook is supposed to run when savin a file.
If the file specified in best-banners-messages-file matches the saved file, we try to create banners"
  (message "save file hook..."))

(add-hook 'after-save-hook 'best-banners--save-file-hook)










(defun new-banner-file-name ()
  (format "./banners/%s" "000"))

(defun banners-create-new (msg)
  (let ((banner (concat banner-header (shell-command-to-string (format "figlet \"%s\"" msg))))
        (banner-file (new-banner-file-name)))
    (write-region banner nil banner-file)))

(defun best-message()
  (let ((msgs (read-lines best-messages-file)))
    (message (rand-element msgs))))

(defun rand-element (list)
  (nth (random (length list)) list))
