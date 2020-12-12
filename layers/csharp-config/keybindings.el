;(require 'c-moves.el)


(spacemacs/declare-prefix-for-mode 'csharp-mode "o" "own")
;; (spacemacs/declare-prefix-for-mode 'csharp-mode "oc" "class")

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "or" 'benj-dotnet-run)
(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oE" 'benj-csharp-exclude-region)
(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oe" 'benj-csharp-exclude-buffer)
(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oo" 'omnisharp-show-overloads-at-point)
(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "ol" 'omnisharp-show-last-auto-complete-result)
(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "os" 'benj-csharp/string-interp-sourrund)




(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "og"
  #'(lambda () (interactive) (require 'c-moves) (my/c-beginning-of-class)))

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oG"
  #'(lambda () (interactive) (require 'c-moves) (my/c-end-of-class)))

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oy"
  #'(lambda () (interactive) (require 'c-moves) (my/csharp-kill-current-class-name)))

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "of" #'csharp-move-back-to-beginning-of-defun)
(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oF" #'csharp-move-fwd-to-end-of-defun)

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode "ob" #'csharp-move-back-to-beginning-of-block)
