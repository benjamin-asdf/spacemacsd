;; snippet util code

(defun team-yassnippets/add-snippet-at-place (file snippet-name line-regex expand-env)
  (team/with-file
   file
   (csharp-mode)
   (team/chsarp-snippet-insert
    snippet-name
    line-regex
    expand-env)))





;; meta code

(defun team-snippets/load-snipptes (path &rest more)
  "Load snippet code located inside this directory.
Purpose is to not load all the stuff always."
  (when path
    (load
     (concat dotspacemacs-directory "layers/team-snippets/" path))
    (team-snippets/load-snipptes (car more) (cdr more))))



(team-snippets/load-snipptes
 "banner-snippets.el"
 )
