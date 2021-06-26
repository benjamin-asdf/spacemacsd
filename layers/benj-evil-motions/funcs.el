(defun benj-avy/angle-bracket-word ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (avy-goto-word-1 ?<)
      (forward-char 1)
      (thing-at-point 'word))))


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

;; TODO find and use the package that does that

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

(defun my/kill-currenty-symbol-overlay-symbols ()
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
        (kill-new
         (buffer-substring
          (overlay-start ov)
          (overlay-end ov))))))



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
  (interactive"r")
  (when
      (= beg end)
    (setq beg (point-at-bol))
    (setq end (point-at-eol)))
  (let ((safe-string
         (lispyville--safe-string beg end nil)))
    (delete-region beg end)
    (insert safe-string)))





(defun take-every-second (list)
  (-non-nil
   (--map-indexed
    (when
        (= (% it-index 2) 1)
      it)
    list)))

(defun benj/second-words (beg end)
  (interactive"r")
  (goto-char beg)
  (let ((s (buffer-substring beg end)))
    (delete-region beg end)
    (insert
     (mapconcat
      #'identity
      (take-every-second
       (s-split " " s))
      " "))))



(defun benj/shed-to-file-name-base ()
  (interactive)
  (let* ((bounds
          (bounds-of-thing-at-point
           'filename))
         (s
          (file-name-base
           (buffer-substring
            (car bounds)
            (cdr bounds)))))
    (delete-region
     (car bounds)
     (cdr bounds))
    (insert s)
    (kill-new s)))
