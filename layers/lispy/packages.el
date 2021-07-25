
(defconst lispy-packages
  '(lispy
    (evil-lispy :location (recipe :fetcher github
                                  :repo "sp3ctum/evil-lispy"
                                  :branch "master"))
    (targets
     :location (recipe
                :fetcher github
                :repo "noctuid/targets.el"))
    lispyville))

(defun lispy/init-lispy ()
  (use-package lispy
    :defer t
    ;; :init (setq lispy-key-theme nil)
    :config (with-eval-after-load
                'evil-mc
                (progn
                  (pushnew
                   'lispy-mode
                   evil-mc-incompatible-minor-modes)
                  (spacemacs|diminish lispy-mode "" "")))))

(defun lispy/init-evil-lispy ()
  (use-package evil-lispy
    :hook ((lisp-mode . lispy-mode)
          (emacs-lisp-mode . lispy-mode)
          (scheme-mode . lispy-mode)
          (racket-mode . lispy-mode)
          (hy-mode . lispy-mode)
          (lfe-mode . lispy-mode)
          (dune-mode . lispy-mode)
          (clojure-mode . lispy-mode))
    :commands (evil-lispy-mode)
    :config (progn
              (spacemacs|diminish evil-lispy-mode " Ⓛ" " L")
              (setq lispy-close-quotes-at-end-p t)
              (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)

              (defadvice lispy-backtick (after my/lispy-lispy-backtick-advice activate)
                (interactive)
                (run-hooks 'post-self-insert-hook))

              (when (configuration-layer/package-usedp 'cider)

                ;; todo better mechanism of loading cider
                (require 'cider)
                ;; show eval results in a cider overlay, next to point
                (add-to-list 'lispy-compat 'cider)
                (setq lispy-eval-display-style 'overlay))

              (with-eval-after-load
                  'python-mode
                  (add-hook
                   'python-mode-hook
                   (lambda () (require 'le-python)))))))

(defun evil-lispy-layer-configure-colorization ()
  ;; this will be displayed in the modeline
  (let ((mode-color "light sea green"))

    (defface spacemacs-lispy-face
      `((t :inherit 'mode-line
           :background ,mode-color))
      "lispy state face."
      :group 'spacemacs)

    (setq evil-lispy-state-cursor '(mode-color box))

    (setq evil-lispy-state-cursor
          (list (when dotspacemacs-colorize-cursor-according-to-state mode-color)
                'box))))

(defun lispy/init-targets ()
  (use-package targets
    :defer t))


(defun lispy/init-lispyville ()
  (use-package lispyville
    :hook (lispy-mode . lispyville-mode)
    :init
    (setq lispyville-key-theme
          '(operators
            c-w
            c-u
            prettify
            slurp/barf-lispy
            additional
            additional-motions
            atom-movement
            commentary
            additional-wrap
            additional-insert
            ;; arrows
            ;; escape
            mark-toggle
            slurp/barf-lispy
            ;; mark-special
            ))
    :config
    (progn

      (setq lispyville-motions-put-into-special t)
      (setq lispyville-commands-put-into-special t)
      ;; (lispyville-enter-visual-when-marking)

      (spacemacs/set-leader-keys
        "C-l"
        #'evil-visual-line)
      (lispyville-set-key-theme)


      (with-eval-after-load
          'cider
          (setf cider-jack-in-dependencies
                (delete-dups
                 (append
                  cider-jack-in-dependencies
                  lispy-cider-jack-in-dependencies))))


      (defalias 'evil-lisp-state-evil-visual-char
        (lispyville-wrap-command lispy-mark-symbol special))
      (defalias 'evil-lisp-state-evil-visual-line
        (lispyville-wrap-command lispy-mark special))


      (require 'targets)
      (setq targets-text-objects nil)
      (targets-setup)
      (targets-define-to lispyville-comment 'lispyville-comment nil object
                         :last-key nil
                         :bind t :keys "c")
      (targets-define-to lispyville-atom 'lispyville-atom nil object
                         :last-key nil
                         :bind t :keys "m")
      (targets-define-to lispyville-list 'lispyville-list nil object
                         :last-key nil
                         :bind t :keys "k")
      (targets-define-to lispyville-sexp 'lispyville-sexp nil object
                         :last-key nil
                         :bind t :keys "x")
      (targets-define-to lispyville-function 'lispyville-function nil object
                         :last-key nil
                         :bind t :keys "f")
      (targets-define-to lispyville-string 'lispyville-string nil object
                         :last-key nil
                         :bind t :keys "S")

      ;; (diminish 'lispyville-mode (lispyville-mode-line-string " 🍰" " 🍰"))
      (spacemacs|diminish lispyville-mode " Ⓥ" " V")

      (with-eval-after-load
          'evil-goggles
        (setq
         evil-goggles--commands
         (append
          evil-goggles--commands
          '((lispyville-delete                :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--generic-blocking-advice)
            (lispyville-delete-line           :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
            (lispyville-yank                  :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
            (lispyville-yank-line             :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
            (lispyville-change                :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
            (lispyville-change-line           :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
            (lispyville-change-whole-line     :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
            (lispyville-join                  :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
            (lispy-fill                       :face evil-goggles-fill-and-move-face         :switch evil-goggles-enable-fill-and-move         :advice evil-goggles--generic-async-advice))))
        (evil-goggles-mode)))))
