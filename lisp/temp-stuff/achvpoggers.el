(defconst move-achv-code-getter-template
  "
        void _updateView() {
            if (!c.TryGetAchvProgressView(%s, out var view)) return;
%s
        }
"
  )


(defun benj-move-achv-code ()
  "Reformat update view code to fit new api."
  (interactive)

  (let ((text) (min) (max) (func-spot) (achvname))
    (while (re-search-forward "AddReactEach(Matcher\.AllOf<\\(\\w+\\)C.*UpdateView>(), e => {" nil t)
      (setq achvname (match-string 1))
      (save-excursion
        (setq func-spot (copy-marker (point-at-eol)))
        (evil-end-of-line)
        (save-excursion
          ;; ommit the "var view =" line
          (forward-line 2)
          (setq min (copy-marker (point-at-bol))))
        (evil-jump-item)
        (save-excursion
          (forward-line -1)
          (setq max (copy-marker (point-at-eol)))))
      (setq text (buffer-substring min max))
      (kill-region (save-excursion
                     (goto-char min)
                     (forward-line -3)
                     (point-at-bol))
                   (save-excursion
                     (goto-char max)
                     (forward-line 1)
                     (point-at-eol)))

      (goto-char func-spot)
      (insert (format move-achv-code-getter-template achvname text))))

  (goto-char (point-min))
  (while (re-search-forward "c.state.UpdateView<\\w+C>();" nil t)
    (replace-match "_updateView();"))
  )


(defconst dirs-with-monos '("/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/Icons/" "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/ProgressBars/"))



(defun benj-achv-replace-obsolete-monos ()
  "Replace syntax for acvh icons and bars, put obsolete attr."
  (interactive)
  (dolist (dir dirs-with-monos)
    (dolist (file (benj-directory-files dir))
      (unless (string-equal "meta" (file-name-extension file))
        (with-temp-file file
          (insert-file-contents-literally file)
          (unless (search-forward "[Obsolete]" nil t)
            (goto-char (point-min))
            (set-buffer-file-coding-system 'utf-8)
            (while (re-search-forward "\r\n" nil t) (replace-match "\n"))
            (if (not (search-forward "Component" nil t))
                (message "Did not find  Component in %s" file)
              (progn (forward-line -2)
                     (insert "using System;\n")
                     (forward-line 1)
                     (insert "\n[Obsolete]")
                     (cond ((re-search-forward "AchievementProgressBar<\\w+>")
                            (replace-match "AchvProgressBar"))
                           ((re-search-forward "AchievementProgressIcon<\\w+>")
                            (replace-match "AchvProgressIcon")))
                     (unless (search-forward "// special")
                       (goto-line 6)
                       (insert "[Obsolete]\n"))))))))))
