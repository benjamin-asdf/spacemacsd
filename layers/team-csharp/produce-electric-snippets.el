
(defun team-electric/produce--h-snippet (file &optional h-string)
  (unless h-string (setf h-string "h"))
  (with-temp-buffer
    (insert-file-contents-literally file)
    (->gg)
    (team/re-replace
     "-m"
     (format "-%s" h-string))
    (->gg)
    (re-search-forward "^\\(#.*?key.*?\\)m$")
    (replace-match (concat "\\1" h-string))
    (team/re-replace
     "team-electric/comp-name"
     (pcase
         h-string
       ("hh" "benj-avy/angle-bracket-word")
       (_ "team-electric/helm-comp-echo")))
    (write-region nil nil
                  (team/re-replace-in-string file
                                             "-m$"
                                             (format "-%s" h-string)))))



(defun team-electric/make-m--snippet (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (re-search-forward
     "#name"
     nil)
    (->$)
    (insert "-m")
    (re-search-forward "#key")
    (->$)
    (insert "m")
    (team/re-replace
     "$0"
     "`(team-electric/comp-name)`")
    (write-region nil nil
                  (concat file "-m"))))


(defun team-electric/make-m-snippet ()
  (interactive)
  (team-electric/make-m--snippet (buffer-file-name)))

(defun team-electric/produce-some-h-snipps ()
  (interactive)
  (-map #'team-electric/produce--h-snippet (directory-files "/home/benj/.spacemacs.d/snippets/csharp-mode/" t "\\-m$"))
  (yas-reload-all))

(defun team-electric/produce-some-hh-snipps ()
  (interactive)
  (--map (team-electric/produce--h-snippet it "hh")
         (directory-files "/home/benj/.spacemacs.d/snippets/csharp-mode/" t "\\-m$"))
  (yas-reload-all))


(provide 'produce-electric-snippets)
