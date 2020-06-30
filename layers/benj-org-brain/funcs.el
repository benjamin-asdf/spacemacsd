












;; https://github.com/Kungsgeten/org-brain





(defun org-brain-cliplink-resource ()
  "Add a URL from the clipboard as an org-brain resource.
Suggest the URL title as a description for resource."
  (interactive)
  (let ((url (org-cliplink-clipboard-content)))
    (org-brain-add-resource
     url
     (org-cliplink-retrieve-title-synchronously url)
     t)))

(define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)




(defface aa2u-face '((t . nil))
  "Face for aa2u box drawing characters")
(advice-add #'aa2u-1c :filter-return
            (lambda (str) (propertize str 'face 'aa2u-face)))
(defun aa2u-org-brain-buffer ()
  (let ((inhibit-read-only t))
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist
                 '(aa2u-face . org-brain-wires))
    (ignore-errors (aa2u (point-min) (point-max)))))
(with-eval-after-load 'org-brain
  (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))




(defun org-brain-deft ()
  "Use `deft' for files in `org-brain-path'."
  (interactive)
  (let ((deft-directory org-brain-path)
        (deft-recursive t)
        (deft-extensions '("org")))
    (deft)))



(defun helm-org-rifle-brain ()
  "Rifle files in `org-brain-path'."
  (interactive)
  (let ((helm-org-rifle-close-unopened-file-buffers nil))
    (helm-org-rifle-directories (list org-brain-path))))

(defun helm-org-rifle-open-in-brain (candidate)
  (-let (((buffer . pos) candidate))
    (with-current-buffer buffer
      (goto-char pos)
      (org-brain-visualize-entry-at-pt))))

(add-to-list 'helm-org-rifle-actions
             (cons "Show entry in org-brain" 'helm-org-rifle-open-in-brain)
             t)
