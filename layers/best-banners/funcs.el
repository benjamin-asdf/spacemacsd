



(defun best-banners--save-file-hook ()
  "This hook is supposed to run when savin a file.
If the file specified in best-banners-messages-file matches the saved file, we try to create banners"
  (message "save file hook..."))

(add-hook 'after-save-hook 'best-banners--save-file-hook)













(defconst best-messages-file "~/.config/my-messages")


(defconst banner-header "\n\nWelcome to spacemacs")

(defun new-banner-file-name ()
  (format "./banners%s" "000"))

(defun create-banner ()
  (let (banner
        (concat banner-header (shell-command-to-string "figlet msg"))
        banner-file (new-banner-file-name))
    (with-temp-file banner-file
      (insert-file-contents banner-file msg))))

(create-banner)

(defun my-best-message ()
  "Get a display message"
  (interactive)
  (substring (shell-command-to-string "fish -c best-message") 0 -1))

(defun best-message()
  (let ((msgs (read-lines best-messages-file)))
    (message (rand-element msgs))))

(defun rand-element (list)
  (nth (random (length list)) list))
