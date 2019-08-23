
(defconst bunel-handle-path "/tmp/bunel-handles")


(defconst bunel-idlegame-projects '("IdleGame" "IdleGameSymbolicLink" "IdleGameSymbolicLink-Extra"))

(defconst bunel-default-unity-project "IdleGame")

;; (defun bunel-refresh-project (&optional project all)
;;   "Refresh idlegame PROJECT. If ALL is non nil, refresh all projects."
;;   (mapcar (lambda (project)
;;             ()))
;;   )

(defun bunel-create-handle-file (project)
  "Create a handle file for project."
  (unless (file-exists-p bunel-handle-path) (make-directory bunel-handle-path))
  (write-region "" nil (concat (file-name-as-directory bunel-handle-path) project)))

(defun bunel-refresh-client ()
  "Refresh default idlegame unity project."
  (interactive)
  (bunel-create-handle-file bunel-default-unity-project))



;; (defun bunel-handle-path ()
;;   (unless bunel-handle-path
;;     (dolist (line (benj-read-lines "~/repos/bunel/.config")) handle-path
;;             (if (string-match "^HANDLE_FILE_PATH=\\(.+\\)$" line)
;;                 (setq handle-path (match-string 1 line)))))
;;   bunel-handle-path)
