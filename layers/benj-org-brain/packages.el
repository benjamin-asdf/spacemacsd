;;; packages.el --- benj-org-brain layer packages file for Spacemacs.
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
;; added to `benj-org-brain-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `benj-org-brain/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `benj-org-brain/pre-init-PACKAGE' and/or
;;   `benj-org-brain/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst benj-org-brain-packages
  '(link-hint
    org-cliplink
    ascii-art-to-unicode
    all-the-icons
    deft
    )
  "The list of Lisp packages required by the benj-org-brain layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format"

  )



(defun benj-org-brain/init-link-hint ()
  (use-package link-hint
    :defer t))


(defun benj-org-brain/org-cliplink ()
  (use-package org-cliplink
    :defer t))

(defun benj-org-brain/ascii-art-to-unicode()
  (use-package ascii-art-to-unicode
    :defer t))

(defun benj-org-brain/all-the-icons()
  (use-package all-the-icons
    :defer t))

(defun benj-org-brain/deft()
  (use-package deft
    :defer t))



;;; packages.el ends here
