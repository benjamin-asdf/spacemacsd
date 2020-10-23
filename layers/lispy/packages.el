
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
    :config (progn
              (pushnew
               'lispy-mode
               evil-mc-incompatible-minor-modes)
              (spacemacs|diminish lispy-mode "" ""))))

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

              (when (configuration-layer/package-usedp 'cider)

                ;; todo better mechanism of loading cider
                (require 'cider)
                ;; show eval results in a cider overlay, next to point
                (add-to-list 'lispy-compat 'cider)
                (setq lispy-eval-display-style 'overlay))

              (defadvice lispy-backtick (after my/lispy-lispy-backtick-advice activate)
                (interactive)
                (run-hooks 'post-self-insert-hook)))))

;; todo yanking marked stuff is broken

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
          '((operators normal)
            c-w
            (prettify insert)
            (atom-movement t)
            slurp/barf-lispy
            additional
            additional-motions
            commentary
            additional-wrap
            additional-insert
            ;; arrows
            escape
            mark
            slurp/barf-lispy))
    :config
    (progn

      (spacemacs/set-leader-keys
        "C-l"
        #'evil-visual-line)

      (lispyville-set-key-theme)
      (require 'targets)
      (setq targets-text-objects nil)
      (targets-setup)
      (targets-define-to lispyville-comment 'lispyville-comment nil object
                         :last-key nil
                         :bind t :keys ";")
      (targets-define-to lispyville-atom 'lispyville-atom nil object
                         :last-key nil
                         :bind t :keys "m")
      (targets-define-to lispyville-list 'lispyville-list nil object
                         :last-key nil
                         :bind t :keys "c")
      (targets-define-to lispyville-sexp 'lispyville-sexp nil object
                         :last-key nil
                         :bind t :keys "x")
      (targets-define-to lispyville-function 'lispyville-function nil object
                         :last-key nil
                         :bind t :keys "f")
      (targets-define-to lispyville-string 'lispyville-string nil object
                         :last-key nil
                         :bind t :keys "S")
      (spacemacs|diminish lispyville-mode " Ⓥ" " V"))))
