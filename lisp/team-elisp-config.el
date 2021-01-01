
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

(use-package
  our-editor-tools
  :init
  (team/spacemacs-declare-keys
      "oe"
      "external"
    "1"
    #'benj/last-editor-msg))


(use-package
  game-log-tools
  ;; :init
  ;; (team/spacemacs-declare-keys
  ;;     "oe"
  ;;     )

  )


(require 'helm-csharp-enums)
(require 'benj-helm-secretary)

(cl-pushnew
 (concat team-elisp-dir "snippets/")
 yas-snippet-dirs)
