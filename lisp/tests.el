(ert-deftest id-when ()
  (assert
   (and
    (id-when 10 #'evenp)
    (id-when 11 #'oddp)
    (not (id-when 11 #'evenp)))))
