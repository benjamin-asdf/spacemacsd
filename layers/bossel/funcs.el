(defconst bossel-handle-dir (concat temporary-file-directory "bossel"))
(defconst bossel-handle-file (concat (file-name-as-directory bossel-handle-dir) "handle"))

(defun bossel--send-package (contents)
  "Send CONTENTS as package to bossel.
CONTENTS must be of valid format TODO"
  (unless (file-exists-p bossel-handle-dir)
    (make-directory bossel-handle-dir))
  (write-region contents nil bossel-handle-file))


(defun bossel--send-current-region ()
  "Development convinience. Send current region as package."
  (interactive)
  (bossel--send-package (buffer-substring (region-beginning) (region-end))))


(defun bossel--encode-package (header body)
  "Format to a package digestable by bossel.
HEADER should be one of the commands defined in "
  (json-encode-list (list header body)))
