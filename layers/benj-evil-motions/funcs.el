;; -*- lexical-binding: t; -*-

(defun benj-avy/jump (arg action &optional beg end)
  (setq avy-all-windows t)
  (avy-with avy-goto-word-0
    (avy-jump avy-goto-word-0-regexp
              :window-flip arg
              :beg beg
              :end end
              :action action)))

(defmacro benj-avy/point-action (&rest body)
  "Return a lambda form that takes one argument, a point.
Then goto hell with safe excursion and eval BODY.
Point arg is anaphorically bound to p."
  `(// (p)
       (save-excursion
         (goto-char p)
         ,@body)))

(defmacro benj-avy/yank-excursion (arg action-form &rest body)
  "Eval ACTION-FORM at avy destination. Use `avy-jump'.
When ACTION-FORM evals to non nil, bind it to it and execute BODY."
  (declare (debug body))
  (declare (indent 2))
  `(let ((it))
     (save-window-excursion
       (benj-avy/jump
        ,arg
        (benj-avy/point-action
         (setq it ,action-form))))
     ,@body))

(defun benj-avy/copy-word (&optional arg)
  (interactive"P")
  (benj-avy/yank-excursion
   arg
   (progn (kill-new (buffer-substring (point) (progn (forward-word) (point)))) t)
   (yank)))

(defun benj-avy/take-word (&optional arg)
  (interactive "P")
  (benj-avy/yank-excursion
   arg
   (progn (kill-word 1)
          (delete-region (point) (progn (forward-word) (point)))
          t)
   (yank)))

(defun benj-avy/move-region (&optional arg)
  (interactive"P")
  (let ((sel))
    (save-excursion
      (benj-avy/jump-timer-action
       arg
       (// (p)
           (print p)
         (setq sel (cons p sel)))))
    ;; (cl-labels
    ;sdfg;     ((put-point (place)
    ;;                 (save-excursion
    ;;                   (benj-avy/jump-timer-action
    ;;                    arg
    ;;                    (// (p) (print p) (setq place (cons p place)))))))
    ;;   (put-point sel)
    ;;   ;; (print sel)
    ;;   ;; (evil-delete (car sel) (cdr sel))
    ;;   ;; (evil-paste-after))
    ;;   )
    )
  )

(defadvice spacemacs/avy-open-url (around my/spacemacs-avy-open-url-adv activate)
  (avy-jump "https?://"
            :action
            (benj-avy/point-action
              (browse-url-at-point))))


(defun benj-avy/jump-timer-action (window-flip action)
  (setq avy-action (or action avy-action))
  (let ((avy-all-windows
         (if window-flip
             (not avy-all-windows)
           avy-all-windows)))
    (avy-with avy-goto-char-timer
      (setq avy--old-cands (avy--read-candidates))
      (avy-process avy--old-cands))))






;; evil mc

(defun my/insert-evil-mc-nums-simple (&optional arg)
  "Insert a number at each evil mc cursor, incremented by cursor index.
Start either at 0 or prefix ARG, if given."
  (interactive"P")
  (let ((num (or arg 0)))
    (evil-mc-execute-for-all-cursors
     (lambda (cursor)
       (insert
        (number-to-string
         (+ num
            (let ((it (evil-mc-get-cursor-property cursor :index)))
              (if (= it 0) (length evil-mc-cursor-list) (- it 1))))))))))


(defun my/evil-mc-on-arglist ()
  "Go into insert state and make a cursor on the end of the arglist of func at point."
  (interactive)
  (evil-mc-run-cursors-before)
  (evil-mc-make-cursor-at-pos
   (save-excursion
     (beginning-of-defun)
     (skip-chars-forward "^)")
     (point)))
  (evil-insert-state))


(defadvice evil-force-normal-state (before my/evil-normal-state-maybe-delete-mc-cursors activ)
  (when (and
         evil-mc-cursor-state
         (eq evil-state 'normal))
    (evil-mc-undo-all-cursors)))



(defun my/region-or-line-bounds ()
  "Return the bounds of the active region, if region is not active return the bounds of the current line instead."
  (or
   (and (region-active-p)
        (car (region-bounds)))
   (cons (point-at-bol) (point-at-eol))))


(defmacro my/with-dwim-region (&rest body)
  "Use bounds as returned by `my/region-or-line-bounds',
Got to the start point, execute body with \"end\" bound to a marker of the end of the dwim region. "
  `(let ((bounds (my/region-or-line-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (let ((end (my/marker-there (cdr bounds))))
        ,@body))))

(defun my/re-replace-dwim (re replace)
  (let (res)
    (my/with-dwim-region
     (while
         (and (> end (point))
              (re-search-forward re end t))
       (replace-match replace)
       (setq res t)))
    res))

(defmacro my/re--toggle-body (left right)
  `(or (my/re-replace-dwim ,left ,right)
      (my/re-replace-dwim ,right ,left)))

(defmacro my/define-re-toggle (left right)
  "Define an interactive func called my/re-toggle-LEFT-RIGHT that swappes the stirng returned by LEFT and RIGHT."
  (declare (indent defun))
  `(defun  ,(symb 'my/re-toggle- left '- right) ()
     (interactive)
     (my/re--toggle-body ,left ,right)))


(my/define-re-toggle
  "pet"
  "hero")
(my/define-re-toggle
  "left"
  "right")
(my/define-re-toggle
  "green"
  "red")
(my/define-re-toggle
  "menu"
  "overlay")
(my/define-re-toggle
  "week"
  "day")
(my/define-re-toggle
  "true"
  "false")
(my/define-re-toggle
  "up"
  "down")
(my/define-re-toggle
  "target"
  "actionButton")
(my/define-re-toggle
  "online"
  "offline")
(my/define-re-toggle
  "window"
  "overlay")

(defun my/re-commata-newline ()
  "Replace occurances of , to a new line in region or line."
  (interactive)
  (my/re--toggle-body "," "\n"))






(defun my/evil-visual-line-around-here ()
  "Search backward and forward stopping at empty lines.
Then select with `evil-visual-line'. "
  (interactive)
  (-some-->
      (re-search-backward "^$" nil t)
    (and (forward-line 1)
         (point))
    (and
     (re-search-forward "^$" nil t)
     (forward-line -1)
     it)
    (evil-visual-line it (point))))

(defun my/evil-mc-make-cursors-around-here ()
  "Use `my/evil-visual-line-around-here' to select a region,
then make cursors"
  (interactive)
  (my/evil-visual-line-around-here)
  (evil-mc-make-cursor-in-visual-selection-beg)
  (evil-force-normal-state))


;; meta the meta
(defvar my/temp-devel-kbd "ott")

(defun my/eval-and-bind-func (&optional arg)
  "Eval func at point. Set keybinding to `my/temp-devel-kbd'.
When ARG is non nil prompt the user for the key binding following the <spcot> leader keys,"
  (interactive"P")
  (spacemacs/set-leader-keys
    (if arg (concat "ot" (read-from-minibuffer "Key: ")) my/temp-devel-kbd)
    (eval-defun nil)))


(defun my/make-cmd-wrapper ()
  "Put a an interactive function defintion
with (symbol at point)."
  (interactive)
  (require 'lispyville)
  (require 'yasnippet)
  (let ((env
         `((name ,(mkstr (symbol-at-point) '-cmd))
           (callee ,(mkstr (symbol-at-point))))))
    (lispyville-forward-function-end)
    (open-line 2)
    (forward-line 1)
    (yas-expand-snippet
     (yas-lookup-snippet
      "command"
      'emacs-lisp-mode)
     nil
     nil
     env))
  (lispyville-backward-function-begin))

(defun my/sexp-to-interactive-func ()
  "Copy the sexp at point, put interactive func snippet below current func."
  (interactive)
  (require 'smartparens)
  (require 'lispyville)
  (let ((env `((my/interactive-func-content
                ,(progn
                   (sp-copy-sexp)
                   (with-temp-buffer
                     (yank)
                     (buffer-string)))))))
    (lispyville-end-of-defun)
    (forward-line 1)
    (open-line 1)
    (forward-line 1)
    (yas-expand-snippet
     (yas-lookup-snippet
      "interactive-func"
      'emacs-lisp-mode)
     nil
     nil
     env))
  (my/eval-and-bind-func))



  ;; bookmarks

  ;; could try out how it would be to have a bookmark foreach workspace
  ;; (defvar my/last-bookmarked-file '())
  ;; (defun my/last-change-bookmark-funtion ()
  ;;   "If buffer is visiting a file different from `my/last-bookmarked-file',
  ;; Store a new book mark named \"last-work\"."
  ;;   (team/a-when
  ;;    (buffer-file-name)
  ;;    (unless (string-equal my/last-bookmarked-file it)
  ;;      (bookmark-set "last-work"))))


  

(defvar my/last-bookmarked-eyebrowse '())
(defvar my/last-bookmarks-lut (make-hash-table))
(defun my/last-change-bookmark-funtion ()
  "If buffer is visiting a file different from `my/last-bookmarked-file',
Store a new book mark named \"last-work\"."
  (team/a-when
   (buffer-file-name)
   (unless
       (string-equal it
                     (gethash my/last-bookmarked-eyebrowse my/last-bookmarks-lut))
     (setf (gethash my/last-bookmarked-eyebrowse my/last-bookmarks-lut) it)
     (let ((name (format "last-work-%s" my/last-bookmarked-eyebrowse)))
       (setq my/last-bookmarked-eyebrowse (eyebrowse--get 'current-slot))
       (bookmark-set name)))))

(defun my/last--bookmark-name (slot)
  (format "last-work-%s" slot))
(defun my/last--jump-bookmark (slot)
  (bookmark-jump (bookmark-get-bookmark (my/last--bookmark-name slot))))

(defun my/jump-last-bookmark ()
  "Jump to the last bookmark made by `my/last-change-bookmark-funtion'."
  (interactive)
  (eyebrowse-switch-to-window-config my/last-bookmarked-eyebrowse)
  (my/last--jump-bookmark my/last-bookmarked-eyebrowse))

(defun my/jump-last-bookmark-this-slot ()
  "Jump to last bookmark made for current eyebrowse slot."
  (interactive)
  (my/last--jump-bookmark (eyebrowse--get 'current-slot)))


;; (add-hook 'post-self-insert-hook
          ;; #'my/last-change-bookmark-funtion)



;; chsarp syntax analysis example
;; (defun my/csharp-eldoc-to-param ()
;;   (interactive)
;;   (team/a-when
;;    team/eldoc-previous-message
;;    (print it)
;; (omnisharp--cs-element-stack-at-point
;;  (let ((type-string it))
;;    (lambda (stack)
;;      (setq best-elm (car (last stack)))
;;      (-let* (((&alist 'Kind kind
;;                       'Ranges ranges) (car (last stack)))
;;              ((&alist 'name  name) ranges)
;;              ((&alist 'Start start
;;                       'End end) name)
;;              ((&alist 'Line line
;;                       'Collumn coll) start))
;;        (->gg)
;;        (forward-line
;;         (- line 1))
;;        (forward-char coll)
;;        (insert "MOFOFO")
;;        )
;;      ;; (--> (car (last stack))
;;      ;;      ))))))





(defun my/comment-or-uncomment-sexpr ()
  "Use evilnc to toggle comment on sexpr."
  (require 'evil-nerd-commenter)
  (interactive)
  (evil-lisp-state-prev-opening-paren)
  (evilnc--invert-comment
   (point)
   (save-excursion
     (evil-jump-item)
     (forward-char 1)
     (when (looking-at-p ")")
       (insert "\n")
       (indent-according-to-mode)
       (forward-line -1))
     (point)))
  (evil-normal-state))



(defvar my/template-string '())
(defvar my/make-template-finish-function '())
(defun my/make-template-string-from-region ()
  "Capture the string on region temporarily.
Create a temp file with the contents of region, the user has the chance to adapt the strign and finalize by
calling `my/make-template-string-from-region' again.
See `my/template-string', which can now be used in lisp.
Also see `my/insert-template-string'."
  (interactive)
  (when my/make-template-finish-function
    (funcall my/make-template-finish-function))
  (let ((file (team-create-temp-file-on-region)))
    (setq
     my/make-template-finish-function
     #'(lambda ()
         (unless (equal file (buffer-file-name))
           (when (yes-or-no-p
                  (format "You are not viisitng %s anymore, quite template session?"
                          file))
             (setq my/make-template-finish-function nil)))
         (setq
          my/template-string (buffer-string)
          my/make-template-finish-function nil)
         (message
          "%s is now %s..."
          (mkstr 'my/template-string)
          (substring-no-properties my/template-string 15))))))

(defun my/insert-template-string (&rest args)
  "Insert a formatted use `my/template-string' as template.
Use `my/make-template-string-from-region' to initialize."
  (team/a-if my/template-string
      (insert (apply #'format `(,my/template-string ,@args)))
    (error "No template string at this point.")))



(defun my/narrow-to-evil-pair ()
  "Narrow the current buffer to what is within the evil pair."
  (interactive)
  (narrow-to-region
   (point)
   (save-excursion
     (evil-jump-item)
     (point))))



(defun my/jump-to-last-symbol-overlay ()
  "Jump to the last created symbol overlay in buffer.
Also call `spacemacs/symbol-overlay-transient-state/body'."
  (interactive)
  (cl-loop
   for ov being the overlays of (current-buffer)
   from (point-min) to (point-max)
   do (when
          (member
           (overlay-get
            ov
            'face)
           symbol-overlay-faces)
        (goto-char
         (overlay-start ov))
        (spacemacs/symbol-overlay-transient-state/body)
        (return))))



(defun my/copy-symbol-other-window ()
  "Insert symobl at other window."
  (interactive)
  (require 'ace-window)
  (forward-char 1)
  (insert
   (save-window-excursion
     (ace-window 0)
     (mkstr (symbol-at-point)))))

(defun insert-arrow ()
  (interactive)
  (insert (make-string 1 8594)))





(defun benj/lispyville-sanitize-region (beg end)
  (require 'lispyville)
  (interactive
   `( ,(region-beginning) ,(region-end)))
  (when
      (= beg end)
    (setq beg (point-at-bol))
    (setq end (point-at-eol)))
  (let ((safe-string
         (lispyville--safe-string beg end nil)))
    (delete-region beg end)
    (insert safe-string)))
