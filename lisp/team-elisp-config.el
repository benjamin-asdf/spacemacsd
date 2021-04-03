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
       (list (expand-file-name (concat team-elisp-dir "snippets/")))))


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
            "h" #'command-history)

          (team/spacemacs-declare-keys "k"
              "lisp"
            "m" #'macrostep-expand)

          (team/spacemacs-declare-keys
              "or"
              "registers etc"
            ;; benj-funcs needs to migrate as well
            ;; "r" #'benj-copy-last-yank-to-register
            "e" #'team/kill-last-eldoc
            "c" #'team/last-eldoc-csharp-no-type)
          (team/spacemacs-declare-keys
              "oj"
              "jump"
            "d" #'benj-find-file-and-line-at-point)))


(use-package
  idlegame-definitions
  :after '(yasnippet csharp-mode)
  :demand
  :config (idlegame-add-charp-yas-hook))

(use-package
  system-stuff
  :init
  (team/spacemacs-declare-keys
      "oe"
      "external"
    "o" #'rider-open-file-at-point))

(use-package
  bunel
  :config
  (load "~/.spacemacs.d/lisp/init-bunel.el"))


(use-package
  log-digest
  :defer t
  :config
  (team/spacemacs-declare-keys
      "oi1"
      "log stuff"
    "l" #'benj-transform-resharper-output))



(use-package benj-file-syncer
  :after csharp-mode
  :demand t
  :config
  (progn
    (team/spacemacs-declare-keys
        "or"
        "remote"
      "r" #'benj-file-syncer-refresh-everything
      "f" #'create-sync-file-req)
    (global-benj-code-patch-client-mode)))

(require 'csharp-config)

(use-package cl-editor-tools)
