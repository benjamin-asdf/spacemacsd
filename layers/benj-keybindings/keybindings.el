;; TODO make this thing here cleaner

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
(spacemacs/set-leader-keys "ojW" 'evil-avy-goto-char-2)

(spacemacs/declare-prefix "om" "move")
(spacemacs/set-leader-keys "omr" 'avy-move-region)
(spacemacs/set-leader-keys "oml" 'avy-move-line)

(spacemacs/declare-prefix "oc" "copy")
(spacemacs/set-leader-keys "ocl" 'avy-copy-line)
(spacemacs/set-leader-keys "ocr" 'avy-copy-region)


(spacemacs/set-leader-keys "ss" 'spacemacs/helm-swoop-region-or-symbol)
(spacemacs/set-leader-keys "sS" 'helm-swoop)

(spacemacs/declare-prefix "og" "git")
(spacemacs/declare-prefix "ogi" "git-idlegame")
(spacemacs/set-leader-keys "ogic" 'benj-make-merge-request-commit)
(spacemacs/set-leader-keys "ogs" 'vc-revision-other-window)
(spacemacs/set-leader-keys "ogc" 'vc-find-conflicted-file)
(spacemacs/set-leader-keys "oga" 'magit-staging)

(spacemacs/set-leader-keys "ogr" (lambda () (interactive) (benj-curr-revision-as-kill nil nil)))
(spacemacs/set-leader-keys "ogR" (lambda () (interactive) (benj-curr-revision-as-kill nil t)))
(spacemacs/set-leader-keys "ogb" (lambda () (interactive) (benj-curr-revision-as-kill t nil)))
(spacemacs/set-leader-keys "ogB" (lambda () (interactive) (benj-curr-revision-as-kill t t)))
(spacemacs/set-leader-keys "ogdf" 'benj-diff-files)
(spacemacs/set-leader-keys "ogfc" 'magit-file-checkout)
(spacemacs/set-leader-keys "ogC" 'benj-quick-commit)



(spacemacs/declare-prefix "ob" "buffer")
(spacemacs/set-leader-keys "obr" 'mikus-reopen-buffer)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys "oxw" 'benj-flush-empty-lines)
(spacemacs/set-leader-keys "oxp" 'benj-delete-some-whitespace)
(spacemacs/set-leader-keys "oxl" 'benj-delete-til-closing-parem)



(spacemacs/set-leader-keys "omi" 'benj-insert-other-line)
(spacemacs/set-leader-keys "omn" 'benj-next-digit)


(define-key evil-insert-state-map (kbd "C-j") 'company-manual-begin)
(define-key evil-insert-state-map (kbd "C-y") 'benj-copy-word-from-above)


(spacemacs/declare-prefix "on" "new")
(spacemacs/set-leader-keys "ons" 'benj-new-shell-script)

(spacemacs/set-leader-keys "orr" 'benj-copy-last-yank-to-register)

(spacemacs/declare-prefix "of" "file")
(spacemacs/set-leader-keys "ofy" 'benj-copy-file-pos-pretty)


(spacemacs/declare-prefix "oo" "org")
(spacemacs/set-leader-keys "oop" 'org-pomodoro)


(spacemacs/declare-prefix "ol" "expand")
(spacemacs/set-leader-keys "olL" 'evil-complete-next-line)
(spacemacs/set-leader-keys "oll" 'evil-complete-next)



(spacemacs/declare-prefix "ow" "system")
(spacemacs/set-leader-keys "ows" 'benj-windows-add-file-to-startup)


;; maybe something
;; (defvar benj-keybindings-prefixes '()
;;   "Should be a list of form '(PREFIX NAME) determining prefixes to be declared.")

;;(push '("obs" "scratch-buff") benj-keybindings-prefixes)


(defconst benj-scratch-buffer-leader-keys "obs")

(spacemacs/declare-prefix benj-scratch-buffer-leader-keys "scratch-buffs")

(progn
  (mapc (lambda (x) (spacemacs/set-leader-keys (concat benj-scratch-buffer-leader-keys (car x)) (cdr x)))
        '(("c" . (lambda () (interactive) (benj--switch-to-scratch-buffer :csharp)))
          ("f" . (lambda () (interactive) (benj--switch-to-scratch-buffer :fundamental)))
          ("l" . (lambda () (interactive) (benj--switch-to-scratch-buffer :lisp-interaction)))
          ("o" . (lambda () (interactive) (benj--switch-to-scratch-buffer :org)))
          ("m" . (lambda () (interactive) (benj--switch-to-scratch-buffer :markdown))))))


(defconst benj-project-leader-keys "op")
(spacemacs/declare-prefix benj-project-leader-keys "project")
(spacemacs/set-leader-keys (concat benj-project-leader-keys "F") 'benj-helm-projectile-find-many-files)
