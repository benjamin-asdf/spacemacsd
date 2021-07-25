;; TODO make this thing here cleaner

(spacemacs/declare-prefix "o" "own")


(team/spacemacs-declare-keys
  "oj"
  "jump"
  "l" #'avy-goto-char-in-line
  "K" #'evil-avy-goto-word-1-above
  "J" #'evil-avy-goto-word-1-below
  "W" #'evil-avy-goto-char-2
  "g" #'spacemacs/jump-to-definition
  "e" #'eshell
)

(team/spacemacs-declare-keys
  "j"
  "jump"
  "e" #'eshell)


(spacemacs/declare-prefix "oc" "copy")
(spacemacs/set-leader-keys "ocl" 'avy-copy-line)
(spacemacs/set-leader-keys "ocr" 'avy-copy-region)


(spacemacs/declare-prefix "ob" "buffer")
(spacemacs/set-leader-keys "obr" 'mikus-reopen-buffer)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys "oxw" 'benj-flush-empty-lines)
(spacemacs/set-leader-keys "oxp" 'benj-delete-some-whitespace)
(spacemacs/set-leader-keys "oxe" 'benj-remove-eol-from-file)
(spacemacs/set-leader-keys "oxE" 'benj-remove-eol)

(define-key evil-insert-state-map (kbd "C-y") 'benj-copy-word-from-above)

(defun my-company-manual-begin ()
  (interactive)
  (if (company-tooltip-visible-p)
      (company-select-next)
    (company-manual-begin)))
(define-key evil-insert-state-map (kbd "C-j") #'my-company-manual-begin)

(evil-define-key 'insert cider-repl-mode-map
  (kbd "C-j") 'spacemacs//clj-repl-wrap-c-j
  (kbd "C-k") 'spacemacs//clj-repl-wrap-c-k)


(spacemacs/declare-prefix "on" "new")
(spacemacs/set-leader-keys "ons" 'benj-new-shell-script)

(spacemacs/set-leader-keys "occ" 'quick-calc)

(spacemacs/declare-prefix "of" "file")
(spacemacs/set-leader-keys "ofy" 'benj-copy-file-pos-pretty)


(spacemacs/declare-prefix "oo" "org")
(spacemacs/set-leader-keys "oop" 'org-pomodoro)
(spacemacs/set-leader-keys "oot" 'benj-msg-time-string)


(spacemacs/declare-prefix "ol" "expand")
(spacemacs/set-leader-keys "olL" 'evil-complete-next-line)
(spacemacs/set-leader-keys "oll" 'evil-complete-next)


(spacemacs/set-leader-keys "off" 'find-file-at-point)


(spacemacs/set-leader-keys "oos" 'spacemacs/symbol-overlay-transient-state/body)

(spacemacs/set-leader-keys "ojb" 'pop-tag-mark)

(spacemacs/set-leader-keys "ojf" 'benj/find-worktree-file-for-buff)

(spacemacs/set-leader-keys "ohf" 'benj/describe-last-function)


(spacemacs/set-leader-keys "ofs" 'benj/open-latest-screenshot)
(spacemacs/set-leader-keys "ofv" #'(lambda () (interactive) (benj/play-vlc (latest-file "~/Videos"))))
(spacemacs/set-leader-keys "ofd" #'(lambda () (interactive) (spacemacs//open-in-external-app (latest-file "~/Downloads"))))


(spacemacs/set-leader-keys "o0i" #'helm-man-woman)

(spacemacs/set-leader-keys "oef" #'with-editor-finish)

(spacemacs/set-leader-keys "obh" (lambda () (interactive) (dired "~/")))

;; maybe something
;; (defvar benj-keybindings-prefixes '()
;;   "Should be a list of form '(PREFIX NAME) determining prefixes to be declared.")

;;(push '("obs" "scratch-buff") benj-keybindings-prefixes)


(defconst benj-scratch-buffer-leader-keys "obs")

(spacemacs/declare-prefix benj-scratch-buffer-leader-keys "scratch-buffs")

(progn
  (mapc (lambda (x) (spacemacs/set-leader-keys (concat benj-scratch-buffer-leader-keys (car x)) (cdr x)))
        '(("s" . (lambda () (interactive) (benj--switch-to-scratch-buffer #'csharp-mode)))
          ;; ("f" . (lambda () (interactive) (benj--switch-to-scratch-buffer :fundamental)))
          ("f" . (lambda () (interactive) (benj--switch-to-scratch-buffer #'text-mode)))
          ("l" . (lambda () (interactive) (benj--switch-to-scratch-buffer #'emacs-lisp-mode)))
          ("o" . (lambda () (interactive) (benj--switch-to-scratch-buffer #'org-mode)))
          ("c" . (lambda () (interactive) (benj--switch-to-scratch-buffer #'common-lisp-mode)))
          ("m" . (lambda () (interactive) (benj--switch-to-scratch-buffer #'markdown))))))
