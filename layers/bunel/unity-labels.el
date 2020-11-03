;;; Code:
(require 'team-utils)

(defun team-unity/get--labels ()
  (->gg)
  (split-string
   (with-output-to-string
     (when (re-search-forward "labels:" nil t)
       (forward-line 1)
       (while (looking-at "- \\(\\w+$\\)")
         (princ (format "%s\n" (match-string-no-properties 1)))
         (forward-line 1))))))

(defun team-unity/add-labels (file-or-meta -labels)
  (team-unity/modify-labels
   file-or-meta
   -labels
   #'-uniq))

(defun team-unity/remove-labels (file-or-meta -labels)
  (team-unity/modify-labels
   file-or-meta
   -labels
   #'-difference))

(defun team-unity/modify-labels (file-or-metal -labels op)
  (team/with-file
   (team-unity/file-or-meta file-or-meta)
   (let* ((curr (team-unity/get--labels))
          (new (funcall op curr -labels)))
     (unless (= (length new) (length curr))
       (team-unity/set--lables new)))))

(defun team-unity/set--lables (-labels)
  "Assume current buffer has meta syntax, change label part to -LABELS."
  (->gg)
  (when (re-search-forward "labels:" nil t)
    (team/delete-this-line)
    (forward-line 1)
    (while (looking-at "-")
      (team/delete-this-line)
      (forward-line 1))
    (->gg))
  (unless (re-search-forward "guid: " nil t)
    (error "Failed to parse meta %s" (buffer-name (current-buffer))))
  (->$)
  (open-line 1)
  (forward-line 1)
  (insert (team-unity/label-syntax -labels)))

(defun team-unity/label-syntax (-labels)
  "Return the label syntax that unity would put for -LABELS."
  (with-output-to-string
    (princ "labels:")
    (dolist (elm -labels)
      (princ (format "\n- %s" elm)))))

(defun team-unity/get-labels (file-or-meta)
  (team/with-file
   (team-unity/file-or-meta file-or-meta)
   (team-unity/get--labels)))

(defun cos/add-labels (files-or-metas -labels)
  "FILES-OR-METAS is a list of files to act upon. -LABELS is a list of labels to add.
If all labels are already defined, do nothing."
  (--each
      files-or-metas
      (cos/with-root
       (team-unity/add-labels
        it
        -labels))))

(defvar cos/prefab-labels-alist
  '((above-topbar . "AboveTopbar")
    (uncategorized . "Uncategorized")
    (normal-overlay . "NormalOverlay")
    (fade-image . "FadeImage")
    (congrats-screen . "CongratsScreen")
    (offline-content . "OfflineContent")
    (online-content . "OnlineContent")))


(defun cos/prefab-label (item)
  (orassoc item cos/prefab-labels-alist))

(defun cos/add-prefab-label-and-add-for-rewrite (file &optional -label)
  (interactive
   `(,(read-file-name "File or meta to add  label")
     ,(completing-read "Label: " (al-values cos/prefab-labels-alist))))
  (cos/add-labels
   (list file)
   (list -label))
  (team/append-line-to-file
   (string-trim-right
    (unity/trim-to-asset-path
     file)
    ".meta")
   (concat idlegame-project-root "prefabs-for-rewrite.txt")))


(team/def-memoized cos/asset-type (file)
  "Return one of online or offline.
FILE should be the thing inside a directory that has unity labels."
  (if (--any?
       (string-equal
        (assoc-default 'online-content cos/prefab-labels-alist)
        it)
       (team-unity/get-labels
        (directory-file-name
         (file-name-directory file))))
      'online
    'offline))

(provide 'unity-labels)
