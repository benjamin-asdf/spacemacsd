
























(defconst move-achv-code-getter-template
  "
            c.TryGetAchvProgressView<%s>(out var view) {
%s
            }
"
  )


(defun benj-move-achv-code ()
  "Reformat update view code to fit new api."
  (interactive)

  (re-search-forward "AddReactEach(Matcher.AllOf<AchievementProgress>(),")
  (let ((text) (min) (max) (achvname))
    (save-excursion
      (re-search-forward "AddReactEach.*UpdateView>(),")
      (evil-end-of-line)
      (save-excursion
        ;; ommit the "var view =" line
        (forward-line 2)
        (setq min (point-at-bol)))
      (evil-jump-item)
      (save-excursion
        (forward-line -1)
        (setq max (point-at-eol))))
    (setq text
          (replace-regexp-in-string
           "\\(^ \\)" "\t"
           (buffer-substring min max)))
    (re-search-forward "c.state.UpdateView<\\(\\w+C\\)>();$")
    (replace-match (format move-achv-code-getter-template (match-string 1) text))

    (re-search-forward "AddReactEach.*UpdateView>(),")
    (kill-region (point-at-bol) (save-excursion ) ))
  )





;; (progn (re-search-forward "c.state.UpdateView<\\(\\w+\\)C>();$")
;;        (replace-match (format best-template (match-string 1)))
;;        )

;; (defconst best-template "hello
;; %s")

;; c.state.UpdateView<ChurchAchievementProgressBarC>();

;; (save-excursion
;;   (forward-line 10)
;;   (point)
;;   )


;; (progn (forward-line 3)
;;        (insert best-str))


;; (count-lines )



;; (setq best-str " best
;;   next line
;; ")

;; best-text


;; (concat ?\n (make-string 4 ?\t) "}")

;; (replace-regexp-in-string "\\(^\\s+\\)" (make-string 4 ?\t ) best-text)


;; (progn (forward-line 2) (insert "something" ?\n "more"))





;; ;; (progn (forward-line) (insert best-text))
;; ;; 				Contexts achC = c.GetAchievementContext(view.GetIdentifier());
;; ;; 				view.UpdateView(achC.activeChurchSlotSpinsAchievement);



;; ;;         (re-search-forward "c.state.UpdateView<\\(\\w+C\\)>();$")
;; ;;         (replace-match (format move-achv-code-getter-template (match-string 1) text)))





;; (progn (re-search-forward "best") (kill-region (point-at-bol) (point-at-eol))
;;        (insert "hello mofos"))


;; "best"
