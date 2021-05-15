(defvar team-elisp-dir "~/repos/lisp/team-elisp/")

(defvar team-enable-slack nil)
(defvar team-enable-gtags nil)

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
