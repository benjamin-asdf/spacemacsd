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
