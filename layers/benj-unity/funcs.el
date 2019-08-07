
(defvar bunel-handle-path nil)

(defun bunel-handle-path ()
  (unless bunel-handle-path
      (dolist (line (read-lines "~/repos/bunel/.config")) handle-path
              (if (string-match "^HANDLE_FILE_PATH=\\(.+\\)$" line)
                  (setq handle-path (match-string 1 line)))))
  bunel-handle-path)



(defconst bunel-default-unity-project "IdleGame")

(defun bunel--handle-create (cmd args project)
  (let*
      ((project (or project bunel-default-unity-project))

       (handle (cons "command" cmd)
               ())
       (handle '(()))
       (handle '(("command" )))
       (handle

        '(("command" . InfuseCrystall) (args meme-plasma restructure-patterns))))
    (write-region
     (json-encode-alist handle) nil )))


(defun params-test (first-param args &rest the-rest)
  (mapcar (lambda (arg) (message arg)) args))


(params-test "hello" '("world" "motherfuckers") 'some-rest 'another-rest)


(write-region (json-encode-alist '(()) nil (bunel-handle-path)))

(write-region (json-encode-alist '((lul . hi))) nil (concat (file-name-as-directory (bunel-handle-path)) "IdleGame"))

(progn
  (unless (file-exists-p (bunel-handle-path))
    (make-directory (bunel-handle-path)))
  (write-region "" nil (concat (file-name-as-directory (bunel-handle-path)) bunel-default-unity-project))
  )


;; (defun crystall-plant ()
;;   (interactive)
;;   (if t
;;       (error "A seed is growing.")
;;     (message "not implemented")))
