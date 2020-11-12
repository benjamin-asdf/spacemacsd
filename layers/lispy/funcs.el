
;;  redefine because the regex did not work for my
;;  Symbol's value but my emacs is putting a different character for the '

(defun lispy--eval-elisp (e-str)
  "Eval E-STR as Elisp code."
  (let ((e-sexp (read e-str)))
    (when (consp e-sexp)
      (cond ((and (memq (car e-sexp) '(defvar defcustom defvar-local))
                  (consp (cdr e-sexp))
                  (boundp (cadr e-sexp)))
             (set (cadr e-sexp) (eval (cl-caddr e-sexp))))
            ((eq (car e-sexp) 'defface)
             (lispy-eval-defun-1 (macroexpand e-sexp)))
            ((memq (car e-sexp) '(\, \,@))
             (setq e-sexp (cadr e-sexp)))))
    (condition-case e
        (prin1-to-string
         (lispy--eval-elisp-form e-sexp lexical-binding))
      (error
       (progn
         (fset '\, nil)
         (let ((es (error-message-string e)))
           (if (and lispy-lax-eval
                    (string-match
                     "^Symbol.s value as variable is void: \\(.*\\)$"
                     es))
               (progn
                 (setq es (match-string 1 es))
                 (set (intern es) nil)
                 (message "Caught unbound variable %s, setting it to nil." es))
             (signal (car e) (cdr e)))))))))
