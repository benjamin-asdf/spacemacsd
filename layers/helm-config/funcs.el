
;; rg
(setq-default helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
(setq-default helm-ag-use-grep-ignore-list 't)
(setq-default helm-candidate-number-limit 100)
(setq-default helm-ag-base-command "rg --color=never --no-heading" )


(defun benj/helm-find-file-recursively ()
  "Recursively find files in glob manner, in the specified directory."
  (interactive)
  (helm-find 'ask-for-dir))



;; swoop advice against big buffers

(advice-add 'helm-swoop :before-while #'benj/helm-swoop-advice)
(defun benj/helm-swoop-advice (&rest args)
  "Meant to advice before-until `helm-swoop'."
  (if (> (line-number-at-pos (point-max)) 10000)
      (progn
        (message "Buffer is a bit big for swoop. Use `spc s f' instead.")
        nil)
    t))





;; helm rg








































;; TEMP hack because somebody fucked up, the helm-aif part can return `t' instead of a number
(defun benj/temp-helm-update-source-p-hack (source)
  "Benj TEMP hack. Whether SOURCE needs updating or not."
  (let ((len (string-width
              (if (assq 'multimatch source)
                  ;; Don't count spaces entered when using
                  ;; multi-match.
                  (replace-regexp-in-string " " "" helm-pattern)
                helm-pattern))))
    (and (or (not helm-source-filter)
             (member (assoc-default 'name source) helm-source-filter))
         (>= len
             (let ((res (helm-aif (assq 'requires-pattern source) (or (cdr it) 1) 0)))
               (or (and (number-or-marker-p res) res) 0)))
         ;; Entering repeatedly these strings (*, ?) takes 100% CPU
         ;; and hang emacs on MacOs preventing deleting backward those
         ;; characters (issue #1802).
         (not (string-match-p "\\`[*]+\\'" helm-pattern))
         ;; These incomplete regexps hang helm forever
         ;; so defer update. Maybe replace spaces quoted when using
         ;; multi-match.
         (not (member (replace-regexp-in-string "\\s\\ " " " helm-pattern)
                      helm-update-blacklist-regexps)))))

(advice-add 'helm-update-source-p :around #'(lambda (orig-func &rest args) (benj/temp-helm-update-source-p-hack (car args))))
