
;; override this fucking shit ESC
(define-key ctl-x-map (kbd "<ESC>" ) nil)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(spacemacs/declare-prefix "o" "own")

(spacemacs/set-leader-keys "ojg" 'spacemacs/jump-to-definition)

;;avy
(spacemacs/declare-prefix "oj" "jump")
(spacemacs/set-leader-keys "ojf" 'avy-goto-char-in-line)
(spacemacs/set-leader-keys "ojK" 'evil-avy-goto-word-1-above)
(spacemacs/set-leader-keys "ojJ" 'evil-avy-goto-word-1-below)
(spacemacs/set-leader-keys "ojt" 'evil-avy-goto-char-timer)

(spacemacs/set-leader-keys "jca" 'evil-avy-goto-char-2)
(spacemacs/set-leader-keys "jcK" 'evil-avy-goto-char-2-above)
(spacemacs/set-leader-keys "jcJ" 'evil-avy-goto-char-2-below)

(spacemacs/declare-prefix "om" "move")
(spacemacs/set-leader-keys "omr" 'avy-move-region)
(spacemacs/set-leader-keys "oml" 'avy-move-line)

(spacemacs/declare-prefix "oc" "copy")
(spacemacs/set-leader-keys "ocl" 'avy-copy-line)
(spacemacs/set-leader-keys "ocr" 'avy-copy-region)

(spacemacs/declare-prefix "os" "search")
(spacemacs/set-leader-keys "osp" 'rg-dwim-project-dir)
(spacemacs/set-leader-keys "osd" 'rg-dwim-current-dir)

(spacemacs/set-leader-keys "ss" 'spacemacs/helm-swoop-region-or-symbol)
(spacemacs/set-leader-keys "sS" 'helm-swoop)

(spacemacs/declare-prefix "og" "git")
(spacemacs/set-leader-keys "ogs" 'vc-revision-other-window)
(spacemacs/set-leader-keys "ogc" 'vc-find-conflicted-file)
(spacemacs/set-leader-keys "oga" 'magit-staging)


(spacemacs/set-leader-keys "op" 'evil-paste-before)


;;TODO fix bug.
;; (spacemacs/set-leader-keys "pf" 'mikus-helm-projectile-find-file)
