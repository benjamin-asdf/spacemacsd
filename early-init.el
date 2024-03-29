(defvar team-enable-slack t)
(defvar team-enable-gtags t)

(defvar team-elisp-dir "~/repos/lisp/team-elisp/")


(dolist
    (file
     (directory-files-recursively
      (concat team-elisp-dir "lisp/")
      ".el"))
  (cl-pushnew
   (expand-file-name
    (file-name-directory file))
   load-path
   :test 'equal))
