

;; patch up some malformed minor mode

(setq
 minor-mode-map-alist
 (--filter
  (let ((pass nil))
    (with-demoted-errors
        (unwind-protect
            (progn
              (setq pass nil)
              (symbol-value (car it))
              (setq pass t))
          pass)))
  minor-mode-map-alist))


;;  org capture is trying to save a buffer not ass. with a file and not handled specially



(add-hook
 'org-capture-before-finalize-hook
 #'(lambda ()
     (org-capture-put :no-save t)))

(add-hook
 'org-capture-after-finalize-hook
 #'(lambda ()
     (with-current-buffer (org-capture-get :buffer) (save-buffer))))


(defun my/temp-fix-omnisharp-requires ()
  (--map
   (team/with-file
    it
    (unless
        (re-search-forward "require 'cl" nil t)
      (forward-line 15)

      (insert
       "
(require 'cl)
(require 'cl-lib)
(require 'csharp-mode)
(require 'json)
(require 'files)
(require 'ido)
(require 'thingatpt)
(require 'dash)
(require 'compile)
(require 'dired)
(require 'popup)
(require 'etags)
(require 'flycheck)
(require 's)
(require 'f)
"))

    )
   (directory-files
    "/home/benj/.emacs.d/elpa/28.0/develop/omnisharp-20201220.906/"
    t
    ".el$"))

  (-map
   #'delete-file
   (directory-files
    "/home/benj/.emacs.d/elpa/28.0/develop/omnisharp-20201220.906/"
    t
    ".elc$")))

(--map #'byte-compile-file
        (directory-files
         "/home/benj/.emacs.d/elpa/28.0/develop/omnisharp-20201220.906/"
         t
         ".el$"))



;; somebody fucked up to put <escape> into kbd

(defvar my-evil-collection-magit-mode-map-bindings
  (let ((states (if evil-collection-magit-use-y-for-yank
                    `(,evil-collection-magit-state visual)
                  `(,evil-collection-magit-state))))
    (append
     `((,states magit-mode-map "g")
       (,states magit-mode-map "\C-j"   magit-section-forward          "n")
       (,states magit-mode-map "gj"    magit-section-forward-sibling  "M-n")
       (,states magit-mode-map "]"     magit-section-forward-sibling  "M-n")
       (,states magit-mode-map "\C-k"   magit-section-backward         "p")
       (,states magit-mode-map "gk"    magit-section-backward-sibling "M-p")
       (,states magit-mode-map "["     magit-section-backward-sibling "M-p")
       (,states magit-mode-map "gr"    magit-refresh                  "g")
       (,states magit-mode-map "gR"    magit-refresh-all              "G")
       (,states magit-mode-map "x"     magit-delete-thing             "k")
       (,states magit-mode-map "X"     magit-file-untrack             "K")
       (,states magit-mode-map "-"     magit-revert-no-commit         "v")
       (,states magit-mode-map "_"     magit-revert                   "V")
       (,states magit-mode-map "p"     magit-push                     "P")
       (,states magit-mode-map "o"     magit-reset-quickly            "x")
       (,states magit-mode-map "O"     magit-reset                    "X")
       (,states magit-mode-map "|"     magit-git-command              ":")
       (,states magit-mode-map "'"     magit-submodule                "o")
       (,states magit-mode-map "\""    magit-subtree                  "O")
       (,states magit-mode-map "="     magit-diff-less-context        "-")
       (,states magit-mode-map "@"     forge-dispatch)
       (,states magit-mode-map "j"     evil-next-line)
       (,states magit-mode-map "k"     evil-previous-line)
       (,states magit-mode-map "gg"    evil-goto-first-line)
       (,states magit-mode-map "G"     evil-goto-line)
       (,states magit-mode-map "\C-d"   evil-scroll-down)
       (,states magit-mode-map "\C-f"   evil-scroll-page-down)
       (,states magit-mode-map "\C-b"   evil-scroll-page-up)
       (,states magit-mode-map ":"     evil-ex)
       (,states magit-mode-map "q"     magit-mode-bury-buffer)

       ;; these are to fix the priority of the log mode map and the magit mode map
       ;; FIXME: Conflict between this and revert. Revert seems more important here
       ;; (,states magit-log-mode-map "-" magit-log-half-commit-limit    "-")
       (,states magit-log-mode-map "=" magit-log-toggle-commit-limit  "=")

       ;; https://github.com/emacs-evil/evil-collection/issues/406
       ;; Use kbd here for S-SPC and S-DEL so evil-collection-define-key can
       ;; parse definition correctly;.
       (,states magit-mode-map ,(kbd "S-SPC") magit-diff-show-or-scroll-up   "SPC")
       (,states magit-mode-map ,(kbd "S-DEL") magit-diff-show-or-scroll-down "DEL")

       ((,evil-collection-magit-state) magit-mode-map ,evil-toggle-key evil-emacs-state)
       ((,evil-collection-magit-state) magit-mode-map ,(kbd "<escape>") magit-mode-bury-buffer))

     (if (eq evil-search-module 'evil-search)
         `((,states magit-mode-map "/" evil-ex-search-forward)
           (,states magit-mode-map "n" evil-ex-search-next)
           (,states magit-mode-map "N" evil-ex-search-previous))
       `((,states magit-mode-map "/" evil-search-forward)
         (,states magit-mode-map "n" evil-search-next)
         (,states magit-mode-map "N" evil-search-previous)))

     `((,states magit-status-mode-map "gz"  magit-jump-to-stashes)
       (,states magit-status-mode-map "gt"  magit-jump-to-tracked)
       (,states magit-status-mode-map "gn"  magit-jump-to-untracked)
       (,states magit-status-mode-map "gu"  magit-jump-to-unstaged)
       (,states magit-status-mode-map "gs"  magit-jump-to-staged)
       (,states magit-status-mode-map "gfu" magit-jump-to-unpulled-from-upstream)
       (,states magit-status-mode-map "gfp" magit-jump-to-unpulled-from-pushremote)
       (,states magit-status-mode-map "gpu" magit-jump-to-unpushed-to-upstream)
       (,states magit-status-mode-map "gpp" magit-jump-to-unpushed-to-pushremote)
       (,states magit-status-mode-map "gh"  magit-section-up                       "^")
       (,states magit-diff-mode-map "gd" magit-jump-to-diffstat-or-diff "j")
       ((visual) magit-diff-mode-map "y" magit-copy-section-value)
       ;; NOTE This is now transient-map and the binding is C-g.
       ;; ((emacs) magit-popup-mode-map "<escape>" "q")
       )

     (when evil-collection-magit-want-horizontal-movement
       `((,states magit-mode-map "H"    magit-dispatch    "h")
         (,states magit-mode-map "L"    magit-log         "l")
         (,states magit-mode-map "\C-l"  magit-log-refresh "L")
         (,states magit-mode-map "h"    evil-backward-char)
         (,states magit-mode-map "l"    evil-forward-char)))

     (when evil-want-C-u-scroll
       `((,states magit-mode-map "C-u" evil-scroll-up)))

     (if evil-collection-magit-use-y-for-yank
         `((,states magit-mode-map "v"    evil-visual-line)
           (,states magit-mode-map "V"    evil-visual-line)
           (,states magit-mode-map "\C-w"  evil-window-map)
           (,states magit-mode-map "y")
           (,states magit-mode-map "yy"   evil-collection-magit-yank-whole-line)
           (,states magit-mode-map "yr"   magit-show-refs            "y")
           (,states magit-mode-map "ys"   magit-copy-section-value   "C-w")
           (,states magit-mode-map "yb"   magit-copy-buffer-revision "M-w")
           ((visual) magit-mode-map "y"   magit-copy-section-value))
       `((,states magit-mode-map "v" set-mark-command)
         (,states magit-mode-map "V" set-mark-command)
         (,states magit-mode-map ,(kbd "<escape>") evil-collection-magit-maybe-deactivate-mark)))

     (when evil-collection-magit-use-z-for-folds
       `((,states magit-mode-map "z")
         (,states magit-mode-map "z1"   magit-section-show-level-1-all)
         (,states magit-mode-map "z2"   magit-section-show-level-2-all)
         (,states magit-mode-map "z3"   magit-section-show-level-3-all)
         (,states magit-mode-map "z4"   magit-section-show-level-4-all)
         (,states magit-mode-map "za"   magit-section-toggle)
         (,states magit-mode-map "zc"   magit-section-hide)
         (,states magit-mode-map "zC"   magit-section-hide-children)
         (,states magit-mode-map "zo"   magit-section-show)
         (,states magit-mode-map "zO"   magit-section-show-children)
         (,states magit-mode-map "zr"   magit-section-show-level-4-all)))))
  "evil-collection-magit bindings for major modes. Each element of this list
takes the form

\(EVIL-STATE MAGIT-MAP NEW-KEY DEF ORIG-KEY\).

ORIG-KEY is only used for testing purposes, and
denotes the original magit key for this command.")

(dolist (binding my-evil-collection-magit-mode-map-bindings)
  (when binding
    (dolist (state (nth 0 binding))
      (evil-collection-define-key
        state (nth 1 binding) (nth 2 binding) (nth 3 binding)))))

(provide 'temp-hacks)
