(defconst best-messages-file "/home/benj/.homesick/repos/dotfiles/home/.config/my-messages")
(defconst banner-header "\n             Welcome to spacemacs.\n")
(defconst banners-dir "~/.spacemacs.d/layers/best-banners/banners")


(defun best-banners--save-file-hook ()
  "This hook is supposed to run when savin a file.
If the file specified in best-banners-messages-file matches the saved file, we try to create banners"
  (when (equal buffer-file-name best-messages-file)
    (my-delete-all-files banners-dir)
    ;; Create new banners for all messages.
    (dolist (msg (read-lines best-messages-file))
      (banners-create-new msg))))

(add-hook 'after-save-hook 'best-banners--save-file-hook)



(defun my-delete-all-files (dir)
  (dolist (elem (directory-files dir))
    (unless (member elem '("." ".."))
      (message elem)
      (delete-file (concat (file-name-as-directory dir) elem)))))

(defun new-banner-file-name (&optional n)
  ;; FIXME
  (let ((n (or n 0))
        (message (type-of n))
        (file (format "%s/%d" banners-dir n)))
    (while (file-exists-p file)
      (setq file (new-banner-file-name (+ 1 n))))
    file))


(defun banners-create-new (msg)
  (let ((banner (concat banner-header (shell-command-to-string (format "figlet \"%s\"" msg))))
        ;; FIXME
        (banner-file (new-banner-file-name 0)))
    (write-region banner nil banner-file)))

(defun best-message()
  (let ((msgs (read-lines best-messages-file)))
    (message (rand-element msgs))))

(defun rand-element (list)
  (nth (random (length list)) list))


;; TODO s
;; random figlet fonts
;; prompt before creating files
;; open a buffer with the progress output
