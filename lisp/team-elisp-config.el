(require 'use-package)

(use-package game-log-tools)

(use-package helm-csharp-enums :defer t)

(use-package helm-stuff :demand)

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


(use-package
  idlegame-definitions
  :after '(yasnippet csharp-mode)
  :config (add-hook
           'csharp-mode-hook
           (lambda ()
             (add-hook
              'yas-after-exit-snippet-hook
              #'idlegame-add-sys-yas-hook
              0
              t))))
