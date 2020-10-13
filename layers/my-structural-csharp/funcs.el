
(defmacro benj/csharp-excurse-and-indent (&rest body)
  "Execute BODY with `save-excursion', afterwards indent all region between point and start point."
  (declare (debug t))
  (declare (indent 2))
  `(save-excursion
    (let ((p (point-marker)))
      ,@body
      (indent-region-line-by-line (min  p (point)) (max p (point))))))

(defun benj/csharp-jump-curly ()
  (interactive)
  (skip-chars-forward "^}"))

(defun benj/chsarp-slurp-bracket-backward (&optional cnt)
  (interactive"P")
  (benj/csharp-slurp-bracket (or cnt -1)))

(defun benj/csharp-slurp-bracket (&optional cnt)
  "Move the next } CNT lines. Negative CNT moves up.;"
  (interactive"P")
  (save-excursion
    (let ((p (point-marker)))
      (re-search-forward "^.*}.*$")
      (let ((s (match-string-no-properties 0)))
        (team/delete-this-line)
        (forward-line (or cnt 1))
        (team/in-new-line s))
      (indent-region-line-by-line p (point)))))

(defun benj/csharp-backwards-slurp-statement (&optional cnt)
  "Move the enclosing bracketed statement CNT lines up."
  (interactive"P")
  (re-search-backward (concat "^.*" (team/regexp-opt "if" "foreach") ".*(.*).*{.*$"))
  (benj/csharp-excurse-and-indent
   (let ((s (match-string-no-properties 0)))
     (team/delete-this-line)
     (forward-line (or cnt -1))
     (team/in-new-line s))))

(defun benj/csharp-forward-expression (&optional cnt)
  "Jump CNT semicolons. If CNT is negative, jump backwards."
  (interactive"P")
  (let ((cnt (or cnt 1)))
    (dotimes (x (abs cnt))
      (forward-char (signum cnt))
      (funcall
       (left-if-negative
        cnt
        #'skip-chars-backward
        #'skip-chars-forward)
       "^;")
      (when (> 0 cnt) (forward-char -1)))))

(defalias 'benj/csharp-backward-expression
  #'(lambda ()
      (interactive)
      (benj/csharp-forward-expression -1)))


;; copy pasta `evil-lisp-state'

(with-eval-after-load
    'evil-core
    (evil-define-state csharp-structural
   "Csharp-Structural state.
 Used to navigate csharp-structural code and manipulate the sexp tree."
   :tag " <L> "
   :suppress-keymap t
   :cursor (bar . 2)
   ;; force smartparens mode
   (if (evil-csharp-structural-state-p) (smartparens-mode))))


(defgroup evil-csharp-structural-state nil
  "Evil csharp-structural state."
  :group 'emulations
  :prefix 'evil-csharp-structural-state-)

(eval-and-compile
  (defcustom evil-csharp-structural-state-global t
    "If non nil evil-csharp-structural-state is available everywhere."
    :type 'boolean
    :group 'evil-csharp-structural-state)

  (defcustom evil-csharp-structural-state-minor-modes '(team/chsarp-superior-mode)
    "Minor modes where evil leader key bindings are defined.
If `evil-csharp-structural-state-global' is non nil then this variable has no effect."
    :type 'sexp
    :group 'evil-csharp-structural-state)



  (defcustom evil-csharp-structural-state-enter-csharp-structural-state-on-command t
    "If non nil, enter evil-csharp-structural-state before executing command."
    :type 'sexp
    :group 'evil-csharp-structural-state))

(defvar evil-csharp-structural-state-default-state 'normal
  "The state to activate when exiting csharp-structural state")

(defmacro evil-csharp-structural-state-enter-command (command)
  "Wrap COMMAND to call evil-csharp-structural-state before executing COMMAND."
  (let ((funcname (if (string-match "csharp-structural-state-"
                                    (symbol-name command))
                      (intern (format "evil-%s" command))
                    (intern (format "evil-csharp-structural-state-%s" command)))))
    `(progn
       (defun ,funcname ()
        (interactive)
        (when evil-csharp-structural-state-enter-csharp-structural-state-on-command
          (evil-csharp-structural-state))
        (call-interactively ',command))
       ',funcname)))

(defun evil-csharp-structural-state-escape-command (command)
  "Wrap COMMAND to escape to normal state before executing COMMAND."
  `(lambda ()
     (interactive)
     (evil-normal-state)
     (call-interactively ',command)))


(defgroup evil-csharp-structural-state nil
  "Evil csharp-structural state."
  :group 'emulations
  :prefix 'evil-csharp-structural-state-)

;; leader maps

(defun evil-csharp-structural-state-leader (leader)
  "Set LEADER."
  (bind-map evil-csharp-structural-state-map
    :evil-use-local t
    :evil-keys (leader)
    :evil-states (normal))
  (eval
   `(bind-map evil-csharp-structural-state-major-mode-map
      :evil-keys (,leader)
      :evil-states (normal)
      :minor-modes ,evil-csharp-structural-state-minor-modes)))

(defun evil-csharp-structural-state/quit ()
  "Quit csharp-structural state and set state `evil-csharp-structural-state-default-state'."
  (interactive)
  (funcall (intern (format "evil-%S-state" evil-csharp-structural-state-default-state))))

(defvar evil-csharp-structural-state-map (make-sparse-keymap))

;; escape
(define-key evil-csharp-structural-state-map [escape] 'evil-csharp-structural-state/quit)
;; toggle csharp-structural state
(define-key evil-csharp-structural-state-map "." 'csharp-structural-state-toggle-csharp-structural-state)

;; hjkl
(define-key evil-csharp-structural-state-map "h" 'evil-backward-char)
(define-key evil-csharp-structural-state-map "j" 'evil-next-visual-line)
(define-key evil-csharp-structural-state-map "k" 'evil-previous-visual-line)
(define-key evil-csharp-structural-state-map "l" 'evil-forward-char)


;; auto-switch to csharp-structural state commands
(defconst evil-csharp-structural-state-commands
  `(
    ("s" . benj/csharp-slurp-bracket)
    ("b" . benj/csharp-backwards-slurp-statement)
    ("j" . benj/csharp-forward-expression)
    ("k" . benj/csharp-backward-expression)
    ("l" . benj/csharp-jump-curly)
    ("1"   . digit-argument)
    ("2"   . digit-argument)
    ("3"   . digit-argument)
    ("4"   . digit-argument)
    ("5"   . digit-argument)
    ("6"   . digit-argument)
    ("7"   . digit-argument)
    ("8"   . digit-argument)
    ("9"   . digit-argument)
    )
  "alist of keys and commands in csharp-structural state.")

(defvar evil-csharp-structural-state-major-mode-map (make-sparse-keymap))

(dolist (x evil-csharp-structural-state-commands)
  (let ((key (car x))
        (cmd (cdr x)))
    (eval
     `(progn
        (if evil-csharp-structural-state-global
            (define-key evil-csharp-structural-state-map ,(kbd key)
              (evil-csharp-structural-state-enter-command ,cmd))
          (define-key evil-csharp-structural-state-major-mode-map ,(kbd key)
            (evil-csharp-structural-state-enter-command ,cmd)))))))


(defun csharp-structural-state-toggle-csharp-structural-state ()
  "Toggle the csharp-structural state."
  (interactive)
  (if (eq 'csharp-structural evil-state)
      (progn
        (message "state: csharp-structural -> normal")
        (evil-normal-state))
    (message "state: %s -> csharp-structural" evil-state)
    (evil-csharp-structural-state)))


(add-hook
 'spacemacs-post-user-config-hook
 #'(lambda ()
     (evil-csharp-structural-state-leader ", k")
     (spacemacs/add-evil-cursor "csharp-structural" "BlueViolet" 'box)))
