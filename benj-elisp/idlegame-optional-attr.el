(defun benj--optional-attr-next-field (&optional arg)
  "Search forward to next field. Crude. If ARG is non nil search backward"
  (search-forward-regexp "\\(public\\|private\\|\\[SerializeField\\]\\) +?\\b\\w+\\b.*;" nil t (if arg -1 nil)))


(defun benj-optional-attr-next-field ()
  "Search forward next field."
  (interactive)
  (benj--optional-attr-next-field))

(defun benj-optional-attr-prev-field ()
  "Search forward next field."
  (interactive)
  (benj--optional-attr-next-field 'prev))

(defun benj-optional-insert-optional-above ()
  "Open line above and insert optional attr."
  (interactive)
  (evil-open-above 1)
  (insert "[OptionalRef]")
  (evil-normal-state)
  (forward-line 2))


(defun benj-optional-insert-optional-on-next ()
  (interactive)
  (benj-optional-attr-next-field)
  (benj-optional-insert-optional-above))

(defun benj-optional-insert-optional-on-prev ()
  (interactive)
  (benj-optional-attr-prev-field)
  (benj-optional-insert-optional-above))

(spacemacs|define-transient-state benj-optional-attr
  :title "Add Optional Ref Transient State"
  :doc "
 Commands^^^^^^
----------------------------
 [_j_/_k_] next / prev field
 [_J_/_K_] next / prev field and instert optional
 [_l_] inster optional attribute
 [_q_] quit "
  :bindings
  ("j" benj-optional-attr-next-field)
  ("J" benj-optional-insert-optional-on-next)
  ("k" benj-optional-attr-prev-field)
  ("K" benj-optional-insert-optional-on-prev)
  ("l" benj-optional-insert-optional-above)
  ("q" nil :exit t))

(spacemacs/set-leader-keys "ooj" 'spacemacs/benj-optional-attr-transient-state/body)
