(defconst surfel-best-search-page "https://g4gsearch.com")
(defvar surfel/process-name "qutebrowser")

(defvar surfel-bookmars
  '()
  "Currently bookmarked urls.")

(defun surfel-run (&rest args)
  "Run surf with ARGS"
  (start-process "surfel" "*surfel*" surfel/process-name (mapconcat 'identity args " ")))

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


;; TODO handle those args
(defun surfel/browse-url-handler (url &rest args)
  "Handler meant to be one of `browse-url-handlers'"
  (surfel-run url))

(let ((reg (regexp-opt (list "^https://" "^http://"))))
  (setq browse-url-handlers '(((regexp-opt ) . surfel/browse-url-handler))))
