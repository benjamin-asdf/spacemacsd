;;; init-geiser.el ---


;;; Commentary:
;;

;;; Code:

(with-eval-after-load
    'geiser-guile
  (setf
   ;; geiser-guile-debug-show-bt-p t
   ;; geiser-guile-jump-on-debug-p t
   ;; geiser-guile-warning-level 'high
   geiser-default-implementation 'guile))

(with-eval-after-load 'macrostep-geiser
  (add-hook
   'geiser-mode-hook
   #'macrostep-geiser-setup))

;; redefine so we just collect the completions once

(defun geiser-completion--read-symbol (prompt &optional default history)
  (let ((minibuffer-local-completion-map geiser-completion--minibuffer-map))
    (make-symbol (completing-read prompt
                                  (geiser-eval--send/result '(:eval (:ge completions "")))
                                  nil nil nil
                                  (or history
                                      geiser-completion--symbol-history)
                                  (or default (geiser--symbol-at-point))))))




(provide 'init-geiser)

;;; init-geiser.el ends here
