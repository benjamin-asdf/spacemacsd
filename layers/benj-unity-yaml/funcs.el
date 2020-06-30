(defconst benj-unity-yaml/prefab-insance-start "^--- !u!1001")

(defun benj-unity-yaml/jump-to-next-prefab-instance ()
  (interactive)
  (re-search-forward benj-unity-yaml/prefab-insance-start nil t))




(define-derived-mode unity-yaml-mode yaml-mode "Unity-Yaml"
  "Mode for unity prefabs."
  :syntax-table nil
  :abbrev-table nil

  ;; etc
  ;; (evil-define-key )



  ;; font lock stuff
  ;; (font-lock-add-keywords nil minder-mode-font-keywords)

  ;; (if (fboundp 'font-lock-flush)
  ;;     (font-lock-flush)
  ;;   (when font-lock-mode
  ;;     (with-no-warnings (font-lock-fontify-buffer)))


  )

(add-to-list 'auto-mode-alist '("\.prefab$" . unity-yaml-mode))

;; todo jump handlers
;; lul could do syntax checkers
