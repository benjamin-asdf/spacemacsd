;; snippet util code

(defun team-yassnippets/add-snippet-at-place (file snippet-name line-regex expand-env)
  (team/with-file
   file
   (csharp-mode)
   (team/csharp-snippet-insert
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



(defun team-snippets/comp-data-class ()
  "Take comp at line and produce a data class expanded snippet."
  (interactive)
  (let* ((class-name (team/comp-name-on-line))
         (field-string
         (save-excursion
           (->0)
           (skip-chars-forward "^(")
           (prog1
            (buffer-substring-no-properties
             (+ (point) 1)
             (save-excursion (skip-chars-forward "^)") (point))))))
         (fields
          (group (split-string
                  (string-trim
                   field-string
                   " ")) 2)))
    (team/csharp-snippet-insert
     "comp-data-class"
     ".*"
     `((name ,class-name))
     1)
    (--map-indexed
     (progn (team-yas/expand-csharp-snippet
             "msg-pack-field"
             `((name ,(string-trim-right (cadr it) ","))
               (type ,(car it))
               (num ,it-index)))
            (team/->new-line)
            (indent-according-to-mode))
     fields)
    (team/->new-line)
    (team-yas/expand-csharp-snippet
     "constructor-data-class"
     `((name ,class-name)
       (params ,field-string)))
    (--map
     (team/in-new-line (format "this.%1$s = %1$s;" (string-trim-right (cadr it) ",")))
     fields)))



(add-to-load-path-if-exists
 "~/.spacemacs.d/layers/team-snippets/"
 )
