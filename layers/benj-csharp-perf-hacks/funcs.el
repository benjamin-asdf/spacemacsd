
;; proof of concept this would be a hammer to make big files zappy
;; other one would be `jit-lock-function'
;; maybe I can have a counter and call c after change every 30 inputs
;; if the buffer exceeds a certain size
;; (advice-add 'c-after-change :around
;;             #'my/c-after-change-adv
;;             )

;; (defun my/c-after-change-adv (orig-func) )
