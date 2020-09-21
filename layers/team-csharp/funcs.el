

(define-minor-mode team/chsarp-superior-mode
  "This mode adds chsarp syntax to strings when you put them on multiple lines."
  :group 'electricity
  (if team/chsarp-superior-mode
      (progn
	      (add-hook 'post-self-insert-hook
                  #'team/csharp-superior-post-self-insert-function
        )


        ;; (font-lock-add-keywords
        ;;  nil
        ;;  '(("\\bMatcher\\b" . speedbar-file-face))

        ;;  )

        )
    (remove-hook 'post-self-insert-hook #'team/csharp-superior-post-self-insert-function)))

(add-hook 'csharp-mode-hook #'team/chsarp-superior-mode)

(defun my-matcher-func (arg)
  (re-search-forward "\\(Matcher\\)\\." arg t)
  )

(defun team/csharp-superior-post-self-insert-function ()
  (when team/chsarp-superior-mode
    (or
     (team/catch-comp-on-line)
     ;; brackets
     (and
      (looking-back "{" (point-at-bol))
      (looking-at "\\([[:blank:]]*)[[:blank:]]*\\(;\\)?\\)\\|\\(.+?\".+?\"\\)\\|\\(.*\"\\)\\|\\([[:blank:]]+$\\)\\|\\(}$\\)\\|\\([[:blank:]]*.+\\)")
      ;; (looking-at "\\([[:blank:]]*)[[:blank:]]*\\(;\\)?\\)\\|\\|\\(.*\\)\\|\\(.*\"\\)\\|\\([[:blank:]]*.+\\)")
      (let ((indent (current-indentation)))
        ;; (dolist (x '(0 1 2 3 4 5 6))
        ;;   (print (format "%d:%s" x (match-string-no-properties x))))
        (unless (or (match-string 4)
                    ;; (match-string 6) TODO single }
                    )
          (if (match-string 1)
             (progn
               (kill-line)
               (team/->new-line)
               (insert "}")
               (yank)
               (unless (looking-back ";")
                 (insert ";"))
               (indent-line-to indent)
               (forward-line -1)
               (goto-char (point-at-eol))
               (team/->new-line)
               (indent-line-to (+ 4 indent))
               (skip-chars-forward "[:blank:]"))
            (if (looking-back "=>[[:blank:]]*{" (point-at-bol))
                (progn
                  (team/->new-line)
                  (insert "}")
                  (forward-char -1)
                  (open-line 1)
                  (forward-line 1)
                  (indent-line-to  indent)
                  (forward-line -1)
                  (indent-line-to (+ indent 4))
                  (skip-chars-forward "[:blank:]"))
              (kill-line)
              (team/->new-line)
              (yank)
              (team/->new-line)
              (indent-line-to indent)
              (insert "}")
              (forward-line -1)
              (skip-chars-forward "[:blank:]")
              (evil-normal-state 1))))))

     ;; strings across lines
     (and
      (looking-back "\n")
      (save-excursion
        (forward-line -1)
        (re-search-forward "\\(.+?\".+?\"\\)\\|\\(.+?\\$\"\\)\\|\\(.+?\"\\)" (point-at-eol) t))
      (or (match-string 2) (match-string 3))
      (let ((indent (current-indentation)))
        (forward-line -1)
        (line->$)
        (insert "\" +")
        (forward-line 1)
        (when (match-string 2)
          (insert "$"))
        (insert "\"")
        (indent-line-to indent))))))



(defvar team-electric/catched-comp '())
(defun team-electric/comp-name ()
  (when team-electric/catched-comp
    (car team-electric/catched-comp)))


(defun team/catch-comp-on-line ()
  "Try search for comp syntax on current line,
if successfull, set to register m and return non nil.
Nil otherwise."
  (interactive)
  (when
   (team/re-this-line
    "public class \\(\\w+\\) : \\(\\w+\\)?Component.*{ }" t)
   (setq
    team-electric/catched-comp
    (list (match-string-no-properties 1)
          (or (match-string-no-properties 2) "Value")))
   (evil-set-register ?m (team-electric/comp-name))))

;; (defun team-electric/catch-comp-at-point ()
;;   "Catch thing at point as comp"
;;   )


(defun team-electric/flag-comp-p (type)
  (string-match-p "Flag" type))

(defmacro team-electric/a-catched-comp (&rest body)
  "When we have a catched comp, bind name and type anaphorically and eval BODY."
  (declare (debug body))
  `(team/a-when
   team-electric/catched-comp
   (cl-destructuring-bind (name type) team-electric/catched-comp
     ,@body)))


(defun team-electric/resolve-cached-comp-set-part ()
  (team-electric/a-catched-comp
   (format
    (if (team-electric/flag-comp-p type)
      "Set<%s>(true)"
      "Add<%s>()")
    name)))

(defun team-electric/copy-word-to-reg ()
  "Copy the current evil word into register m."
  (interactive)
  (evil-set-register ?m (thing-at-point 'evil-word)))
