(require 'use-package)

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


(use-package game-log-tools)

(use-package helm-csharp-enums :defer t)

(use-package
  benj-helm-secretary
  :demand
  :init
  (progn
    (team/spacemacs-declare-keys
        "os"
        "search/secretary/strings"
      "d" #'secretary-collect)))


(setq yas-snippet-dirs
      (append
       yas-snippet-dirs
       (list (concat team-elisp-dir "snippets/"))))


(use-package
  idlegame-definitions
  :init
  (progn
    (team/spacemacs-declare-keys
        "oi0"
        "tools"
      "f" #'my/cos-jump-asset-file)

    (team/spacemacs-declare-keys
        "oig"
        "cos-git"
      "O" #'cos/reset-hard)))

(use-package
  our-editor-tools
  :init (progn
          (team/spacemacs-declare-keys
              "os"
              "search/secretary/strings"
            "e" #'string-edit-at-point)
          (team/spacemacs-declare-keys
              "o0"
              "utils"
            "r" #'team/regex-builder-with-region
            "h" #'command-history
            "m" #'macrostep-expand)

          (team/spacemacs-declare-keys
              "or"
              "registers etc"
            ;; benj-funcs needs to migrate as well
            "r" #'benj-copy-last-yank-to-register
            "e" #'team/last-eldoc-csharp-no-type)))
