(defcustom mind-palacer-repo-dir "~/repos/mind-palace/"
  "The path of the directory in which mind palacer expects a mind palace repo. It should include a PO dir in the correct formatting")

(defconst mind-palacer-po-dir (concat mind-palacer-repo-dir "PO"))


(defun mind-palacer--dir-for-num (num)
  (let ((s (number-to-string num)))
    (format "%s/%s" (substring s 0 1) (substring s 0 (- (length s) 1)))))


(defun mind-palacer--num-file-name (num)
  (let ((dir (file-name-as-directory (concat (file-name-as-directory mind-palacer-po-dir) (mind-palacer--dir-for-num num)))))
    (car (directory-files dir t (format "^%d_" num)))))




;; TODO
;; something where I navigate  to the file using j,k,l etc



;; create some kind of map -> num and navigation keys


;; miner mode that listenes for navigation keys


;; more features to the minor mode
