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
  benj-home-row-state
  :defer t
  :init (define-key evil-normal-state-map (kbd ", ;") 'benj-toggle-home-row-numbers-state))

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
  ;; TODO autoloads
  ;; :defer t
  :demand
  :config
  (team/spacemacs-declare-keys
      "oi1"
      "log stuff"
    "l" #'benj-transform-resharper-output))



(use-package benj-file-syncer
  :demand t
  :config
  (progn
    (team/spacemacs-declare-keys
        "or"
        "remote"
      "r" #'benj-file-syncer-refresh-everything
      "f" #'benj-syncer-create-file-req 
      "j" #'benj-cos-trace-method
      "." #'benj-send-bunel-cmd-to-hallway
      "k" #'benj-add-to-trace-log
      "K" #'benj-add-contexts-name-log
      "d" #'benj-code-patch-dispatch
      "{" #'benj-csharp-add-curly-brackets
      "a" #'airtest-run-dwm)))

(use-package benj-file-patch-additionals
  :after benj-file-syncer
  :demand t
  :config
  (progn
    (benj-code-patcher-spinner-setup)
    (global-benj-code-patch-client-mode)))




(require 'csharp-config "~/.spacemacs.d/lisp/csharp-config.el")



(use-package
    benj-more-motions
  :bind ("<space> o m n" . benj/insert-evil-mc-nums-simple))

(use-package team-git
  :demand t
  :config (load "~/.spacemacs.d/lisp/init-team-git.el"))

(use-package listful
  :after csharp-mode
  :demand t
  :config
  (team/spacemacs-declare-keys
      "ol" "listful"
    "s" #'listful-start
    "f" #'listful-finish
    "u" #'listful-update))

(use-package cl-editor-tools)
