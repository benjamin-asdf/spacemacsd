(use-package sayid
  :defer t
  :straight (:host github :repo "clojure-emacs/sayid")
  :init
  (progn
    (setq sayid--key-binding-prefixes
          '(("mdt" . "trace")))
    (spacemacs|forall-clojure-modes m
      (mapc (lambda (x) (spacemacs/declare-prefix-for-mode m
                          (car x) (cdr x)))
            sayid--key-binding-prefixes)
      (spacemacs/set-leader-keys-for-major-mode m
        ;;These keybindings mostly preserved from the default sayid bindings
        "d!" 'sayid-load-enable-clear
        "dE" 'sayid-eval-last-sexp ;in default sayid bindings this is lowercase e, but that was already used in clojure mode
        "dc" 'sayid-clear-log
        "df" 'sayid-query-form-at-point
        "dh" 'sayid-show-help
        "ds" 'sayid-show-traced
        "dS" 'sayid-show-traced-ns
        "dtb" 'sayid-trace-ns-in-file
        "dtd" 'sayid-trace-fn-disable
        "dtD" 'sayid-trace-disable-all
        "dte" 'sayid-trace-fn-enable
        "dtE" 'sayid-trace-enable-all
        "dtK" 'sayid-kill-all-traces
        "dtn" 'sayid-inner-trace-fn
        "dto" 'sayid-outer-trace-fn
        "dtp" 'sayid-trace-ns-by-pattern
        "dtr" 'sayid-remove-trace-fn
        "dty" 'sayid-trace-all-ns-in-dir
        "dV" 'sayid-set-view
        "dw" 'sayid-get-workspace
        "dx" 'sayid-reset-workspace))


    (evilified-state-evilify sayid-mode sayid-mode-map
      (kbd "H") 'sayid-buf-show-help
      (kbd "n") 'sayid-buffer-nav-to-next
      (kbd "N") 'sayid-buffer-nav-to-prev
      (kbd "C-s v") 'sayid-toggle-view
      (kbd "C-s V") 'sayid-set-view
      (kbd "L") 'sayid-buf-back
      (kbd "e") 'sayid-gen-instance-expr) ;Originally this was bound to 'g', but I feel this is still mnemonic and doesn't overlap with evil

    (evilified-state-evilify sayid-pprint-mode sayid-pprint-mode-map
      (kbd "h") 'sayid-pprint-buf-show-help
      (kbd "n") 'sayid-pprint-buf-next
      (kbd "N") 'sayid-pprint-buf-prev
      (kbd "l") 'sayid-pprint-buf-exit)

    (evilified-state-evilify sayid-traced-mode sayid-traced-mode-map
      (kbd "l") 'sayid-show-traced
      (kbd "h") 'sayid-traced-buf-show-help))
  :config
  (progn
    ;; If sayid-version is null the .elc file
    ;; is corrupted. Then force a reinstall and
    ;; reload the feature.
    (when (null sayid-version)
      (package-reinstall 'sayid)
      (unload-feature 'sayid)
      (require 'sayid)
      (setq cider-jack-in-lein-plugins (delete `("com.billpiel/sayid" nil) cider-jack-in-lein-plugins)))))





(defadvice cider-jump-to-compilation-error
    (around my-cider-next-err-adv (&optional args reset) activate)
  (let ((p (point)))
    ad-do-it
    (when (eq p (point))
      (message "")
      (flycheck-next-error))))



(provide 'init-cider)
