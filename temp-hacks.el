

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


(provide 'temp-hacks)
