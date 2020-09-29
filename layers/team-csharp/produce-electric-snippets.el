
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

(-map #'team-electric/produce--h-snippet (directory-files "/home/benj/.spacemacs.d/snippets/csharp-mode/" t "\\-m$"))
