
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





(defun my/re-left-right ()
  "Replace occurances of left to right in active region."
  (interactive)
  (unless (region-active-p) (user-error "Region not active"))
  (save-excursion
    (goto-char (region-beginning))
    (while
        (re-search-forward "left" (save-excursion
                                    (progn (goto-char (region-end))
                                           (my/marker-there (point-at-eol)))) t)
     (replace-match "right"))))



;; meta the meta

;; TODO eval , e f and this at once
(defvar my/temp-devel-kbd "ott")
(defun my/assign-temp-kbd ()
  "Assume `symbol-at-point' is a command and assign `my/temp-devel-kbd' to it."
  (interactive)
  (spacemacs/set-leader-keys my/temp-devel-kbd (symbol-at-point)))
