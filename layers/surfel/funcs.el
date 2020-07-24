(defconst surfel/best-search-page "https://g4gsearch.com")
(defconst surfel/default-search-page "https://search.snopyta.org")
(defconst surfel/default-search-page-input "https://search.snopyta.org/?q=%s")

(defvar surfel/process-name "qutebrowser")
(defconst surfel/moszen-types '("sea" "river" "lake"  "jungle" "forest" "wheather"))

(defvar surfel/bookmars
  '()
  "Currently bookmarked urls.")

(defun surfel/run (&rest args)
  "Run surf with ARGS"
  (start-process "surfel" "*surfel*" surfel/process-name (mapconcat 'identity args " ")))

;; TODO get the current url from some running process
;; TODO use qute browser bookmarks
(defun surfel/add-bookmark (url)
  "Add "
  (push url surfel/bookmars))


(defun surfel/open-best-trello-board ()
  (interactive)
  (surfel/run "https://trello.com/b/5e62bac44a00757bfd0fdfe4"))


(defun surfel/search (&optional arg)
  "Open google."
  (interactive)
  ;;(surfel/run "https://start.duckduckgo.com/")
  (surfel/run (if arg (format surfel/default-search-page-input arg) surfel/default-search-page)))

(defun surfel/run-with-yank ()
  "Run browser with latest yank as url"
  (interactive)
  (surfel/run (evil-get-register ?\")))

(defun surfel/moszen-open-rand ()
  "Open a random moszen page."
  (interactive)
  (surfel/run (concat "https://www.moszen.com/" (rand-element surfel/moszen-types))))


;; TODO handle those args
(defun surfel/browse-url-handler (url &rest args)
  "Handler meant to be one of `browse-url-handlers'"
  (surfel/run url))

(defun surfel/search-for-region ()
  "Open browser with current region."
  (interactive)
  (when (region-active-p)
    (surfel/search (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun surfel/search-word-at-point ()
  "start browser searching for the evil WORD."
  (interactive)
  (surfel/search (thing-at-point 'evil-WORD)))

(defun surfel/search-last-flycheck-message ()
  "Depends on `benj-flycheck/display-error-messages-advice'."
  (interactive)
  (when
      (and
       (boundp benj-flycheck/last-error-messages)
       benj-flycheck/last-error-messages)
    (surfel/search benj-flycheck/last-error-messages)))

(defun surfel/search-last-eldoc-message ()
  "Search last eldoc message, if set. Depends on `team/eldoc-save-last-message'."
  (when
      (and
       (boundp team/eldoc-previous-message)
       team/eldoc-previous-message)
    (surfel/search team/eldoc-previous-message)))

;; TODO how does that work?
;; (let ((reg (regexp-opt (list "^https://" "^http://"))))
;;   (setq browse-url-handlers (list (list reg 'surfel/browse-url-handler))))

(setq browse-url-handlers '(("^https?://" . surfel/browse-url-handler)))
