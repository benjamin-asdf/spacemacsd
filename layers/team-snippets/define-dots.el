(defconst cos/dot-prestine-commit "cb5747eb8e204a653c8f7cf46cb3e2e587102409")
(defconst cos/dot-source-file (concat cos-dir "/IdleGame/Assets/#/Sources/GameGuide/DotSource.cs"))
;; (defconst cos/dot-ext (concat cos-dir "/IdleGame/Assets/#/Sources/GameGuide/DotSourceDailyRefreshExt.cs"))

(defvar cos/dots-definitions '())

(defun cos/define-dot (name &key re-add-on-daily)
  (team/with-file
   cos/dot-source-file
   (unless (re-search-forward name nil t)
     (csharp-mode)
     (re-search-forward "//Red dots have offset of 10.000" nil)
     (forward-line -2)
     (team/in-new-line (format "%s," name))
     (when re-add-on-daily
       (re-search-forward "ReaddOnDailyRefresh" nil t)
       (forward-line 1)
       (->$)
       (team/in-new-line (format "case DotSource.%s:" name))))))

(defun cos/revert-dots-definitions ()
  (team/with-default-dir
   cos-dir
   (magit-file-checkout cos/dot-prestine-commit cos/dot-source-file)))

(defun cos/generate-dots ()
  (interactive)
  (cos/revert-dots-definitions)
  (load "/home/benj/.spacemacs.d/layers/team-snippets/dots-definitions.el"))

(defun my/remove-green ()
  (interactive)
  (team/^$-replace
   "Green"
   ""))

(team/spacemacs-declare-keys
 "ot"
 "temp"
 "f" #'my/remove-green
 )

(with-current-buffer-window
    "f"
    nil
    nil
 (insert
  (mapconcat 'identity
             (--zip-with
              (with-output-to-string
                (print `(cos/define-dot ,(format "%s" it) :re-add-on-daily ,(string-equal "Yes" other))))
              (team/file-lines
               "/home/benj/idlegame/IdleGame/Assets/#/Sources/GameGuide/names")
              (team/file-lines
               "/home/benj/idlegame/IdleGame/Assets/#/Sources/GameGuide/readd")) "\n")))

(with-current-buffer-window
    "f"
    nil
    nil
  (erase-buffer)
  (insert
   (with-output-to-string
     (--map
      (print `(cos/make-dot-system ,it))
      (team/file-lines
       "/home/benj/idlegame/IdleGame/Assets/#/Sources/GameGuide/names")))))


(provide 'define-dots)
