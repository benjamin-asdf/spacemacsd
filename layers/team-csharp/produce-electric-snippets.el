
(defun team-electric/produce--h-snippet (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (->gg)
    (team/re-replace
     "-m"
     "-h")
    (->gg)
    (re-search-forward "^\\(#.*?key.*?\\)m$")
    (replace-match "\\1h")
    (team/re-replace
     "team-electric/comp-name"
     "team-electric/helm-comp-echo")
    (write-region nil nil
                  (team/re-replace-in-string file
                                             "-m$"
                                             "-h"))))



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

(provide 'produce-electric-snippets)

;; (-map #'team-electric/produce--h-snippet (directory-files "/home/benj/.spacemacs.d/snippets/csharp-mode/" t "\\-m$"))
