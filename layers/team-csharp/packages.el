;;; packages.el --- team-csharp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: benj <benj@benj-pc>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `team-csharp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `team-csharp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `team-csharp/pre-init-PACKAGE' and/or
;;   `team-csharp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst team-csharp-packages '())

;; (add-to-load-path-if-exists "~/.spacemacs.d/layers/team-csharp/")
;; (require 'cos-comp-types)

;;; packages.el ends here
