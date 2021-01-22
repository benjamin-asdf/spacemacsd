

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


(defun my/temp-nullify-app-sync ()
  (interactive)
  (team/with-file
   "/home/benj/idlegame/AppSyncUnityProject/Packages/AwsAppSync/Runtime/AppSyncSystems.cs"
   (while
       (re-search-forward
        "Add(new.*System"
        nil
        t)
     (replace-match "//"))))


(defun my/temp-fix-omnisharp-requires ()
  (--map
   (team/with-file
    it
    (unless
        (re-search-forward "require 'cl" nil t)
      (forward-line 15)

      (insert
       "
(require 'cl)
(require 'cl-lib)
(require 'csharp-mode)
(require 'json)
(require 'files)
(require 'ido)
(require 'thingatpt)
(require 'dash)
(require 'compile)
(require 'dired)
(require 'popup)
(require 'etags)
(require 'flycheck)
(require 's)
(require 'f)
"))

    )
   (directory-files
    "/home/benj/.emacs.d/elpa/28.0/develop/omnisharp-20201220.906/"
    t
    ".el$"))

  (-map
   #'delete-file
   (directory-files
    "/home/benj/.emacs.d/elpa/28.0/develop/omnisharp-20201220.906/"
    t
    ".elc$")))








(provide 'temp-hacks)
