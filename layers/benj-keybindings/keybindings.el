;; override some ESC stuff
(define-key ctl-x-map (kbd "<ESC>" ) nil)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))



(spacemacs/declare-prefix "o" "own")

(spacemacs/set-leader-keys "ojg" 'spacemacs/jump-to-definition)

;;avy
(spacemacs/declare-prefix "oj" "jump")
(spacemacs/set-leader-keys "ojl" 'avy-goto-char-in-line)
(spacemacs/set-leader-keys "ojK" 'evil-avy-goto-word-1-above)
(spacemacs/set-leader-keys "ojJ" 'evil-avy-goto-word-1-below)
(spacemacs/set-leader-keys "ojt" 'evil-avy-goto-char-timer)

(spacemacs/declare-prefix "om" "move")
(spacemacs/set-leader-keys "omr" 'avy-move-region)
(spacemacs/set-leader-keys "oml" 'avy-move-line)

(spacemacs/declare-prefix "oc" "copy")
(spacemacs/set-leader-keys "ocl" 'avy-copy-line)
(spacemacs/set-leader-keys "ocr" 'avy-copy-region)


(spacemacs/set-leader-keys "ss" 'spacemacs/helm-swoop-region-or-symbol)
(spacemacs/set-leader-keys "sS" 'helm-swoop)

(spacemacs/declare-prefix "og" "git")
(spacemacs/set-leader-keys "ogs" 'vc-revision-other-window)
(spacemacs/set-leader-keys "ogc" 'vc-find-conflicted-file)
(spacemacs/set-leader-keys "oga" 'magit-staging)

(spacemacs/declare-prefix "ob" "buffer")
(spacemacs/set-leader-keys "obr" 'mikus-reopen-buffer)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys "oxw" 'benj-flush-empty-lines)

(spacemacs/set-leader-keys "omi" 'benj-insert-other-line)


(define-key evil-insert-state-map (kbd "C-j") 'company-manual-begin)
(define-key evil-insert-state-map (kbd "C-y") 'benj-copy-word-from-above)


(spacemacs/declare-prefix "on" "new")
(spacemacs/set-leader-keys "ons" 'benj-new-shell-script)