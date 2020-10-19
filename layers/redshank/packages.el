;;; packages.el --- redshank layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: benj <benj@benj-pc>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst redshank-packages
  '((redshank :location local)))

(defconst redshank-major-modes
  '(lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    common-lisp-mode))

(defun redshank/init-redshank ()
  (require 'redshank-loader)
  (redshank-setup
   (--map
    (symb (mkstr it '-hook))
    redshank-major-modes))
  (require 'redshank)

  (dolist (mode redshank-major-modes)
    (spacemacs/declare-prefix-for-mode
      mode "r" "refactor")
    (spacemacs/declare-prefix-for-mode
      mode "rr" "redshank")
    (--each
        redshank-keys
      (cl-destructuring-bind (key . def) it
        (spacemacs/set-leader-keys-for-major-mode
          mode
          (concat "rr" key)
          def))))
  (spacemacs|diminish redshank-mode " Ⓡ" " R"))


;;; packages.el ends here
