
(defvar surfel-bookmars
  '()
  "Currently bookmarked urls.")

(defun surfel-run (&rest args)
  "Run surf with ARGS"
  (start-process "surf" "*surf*" "surf" (mapconcat 'identity args " ")))

;; TODO get the current url from some running process
(defun surfel-add-bookmark (url)
  "Add "
  (push url surfel-bookmars))


(defun surfel-open-best-trello-board ()
  (interactive)
  (surfel-run "https://trello.com/b/5e62bac44a00757bfd0fdfe4"))


;; (surfel-run "google.com")

(defun surfel-google ()
  "Open google."
  (interactive)
  (surfel-run "google.com"))

(defun surfel-run-with-yank ()
  "Run browser with latest yank as url"
  (interactive)
  (surfel-run (evil-get-register ?\")))


