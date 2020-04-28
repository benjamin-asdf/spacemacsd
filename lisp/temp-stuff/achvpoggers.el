(defconst move-achv-code-getter-template
  "
        void _updateView() {
            if (!c.TryGetAchvProgress%sView(AchvViewId.%s, out var view)) return;
%s
        }
"
  )


(defconst achv-file "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/AchievementProgressDisplaySystems.cs")

(benj-move-achv-code-perf nil)

(defun benj-move-achv-code (file)
  "Reformat update view code to fit new api."
  (interactive"fFile to rewrite or default: ")
  (setq file (or file achv-file))
  (with-temp-file file
    (insert-file-contents-literally file)
    (benj-remove-eol-in-buff)
    (let ((text) (min) (max) (func-spot) (achvname) (kindname))
      (while (re-search-forward "AddReactEach(Matcher\.AllOf<\\(\\w+\\)C.*UpdateView>(), e => {" nil t)
        (setq achvname (match-string 1))
        (setq kindname (if (string-match "Bar" achvname) "Bar" "Icon"))
        (save-excursion
          (setq func-spot (copy-marker (point-at-eol)))
          (evil-end-of-line)
          (save-excursion
            (forward-line 1)
            (setq min (copy-marker (point-at-bol))))
          (evil-jump-item)
          (save-excursion
            (forward-line -1)
            (setq max (copy-marker (point-at-eol)))))
        (setq text (buffer-substring min max))
        (kill-region (save-excursion
                       (goto-char min)
                       (search-backward "AddReactEach")
                       (point-at-bol))
                     (save-excursion
                       (goto-char max)
                       (forward-line 1)
                       (point-at-eol)))

        (goto-char func-spot)
        (insert (format move-achv-code-getter-template kindname achvname text))))

    (goto-char (point-min))
    (while (re-search-forward "c.state.UpdateView<\\w+C>();" nil t)
      (replace-match "_updateView();"))

    (goto-char (point-min))
    (while (re-search-forward "^.*var view =.*Get<\\w+Progress\\w+>()\.value.*" nil t)
      (goto-char (point-at-bol))
      (kill-line t))))



(defconst dirs-with-monos '("/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/Icons/" "/home/benj/idlegame/IdleGame/Assets/#/Sources/Achievements/MonoBehaviours/AchievementProgressDisplayImpl/ProgressBars/"))


(defun benj-remove-eol-in-buff ()
  "Remove eol and aset to ut8."
  (interactive)
  (save-excursion (goto-char (point-min))
                  (while (re-search-forward "\r\n" nil t) (replace-match "\n"))))




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
            (save-excursion (while (re-search-forward "\r\n" nil t) (replace-match "\n")))
            (goto-char (point-min))
            (if (not (search-forward "Component" nil t))
                (message "Did not find  Component in %s" file)
              (progn (forward-line -2)
                     (insert "using System;\n")
                     (forward-line 1)
                     (insert "\n[Obsolete]")
                     (cond ((re-search-forward "AchievementProgressBar<\\w+>" nil t)
                            (replace-match "AchvProgressBar"))
                           ((re-search-forward "AchievementProgressIcon<\\w+>" nil t)
                            (replace-match "AchvProgressIcon")))
                     (unless (search-forward "// special" nil t)
                       (goto-line 6)
                       (insert "[Obsolete]\n"))))))))))
