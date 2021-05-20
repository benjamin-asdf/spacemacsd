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


(provide 'init-geiser)

;;; init-geiser.el ends here
