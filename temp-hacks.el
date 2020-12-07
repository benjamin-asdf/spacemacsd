

;; patch up some malformed minor mode

(setq
 minor-mode-map-alist
 (--filter
  (let ((pass nil))
    (with-demoted-errors
        (unwind-protect
            (progn
              (setq pass nil)
              (symbol-value (car it))
              (setq pass t))
          pass)))
  minor-mode-map-alist))


;;  org capture is trying to save a buffer not ass. with a file and not handled specially



(add-hook
 'org-capture-before-finalize-hook
 #'(lambda ()
     (org-capture-put :no-save t)))

(add-hook
 'org-capture-after-finalize-hook
 #'(lambda ()
     (with-current-buffer (org-capture-get :buffer) (save-buffer))))

(provide 'temp-hacks)
