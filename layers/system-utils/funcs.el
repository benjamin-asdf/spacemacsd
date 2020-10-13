;; -*- lexical-binding: t; -*-


(add-to-load-path-if-exists
 "~/.spacemacs.d/layers/system-utils/funcs.el"
 )




;; (defun my/lazy-update-rider ()
;;   "Search in ~/Downloads for a rider tar, extract to /opt/rider, make symlink."
;;   (interactive)
;;   (require 'lazy-scripts)
;;   (my/update-rider))


(defun my/amixer-change-volume (perc up)
  "PERC is a number to change volume,
IF UP is non nil go up, else go down."
  (shell-command (format "amixer -D pulse sset Master %d%%%s"
                         perc
                         (if up "+" "-"))))

(defvar my/amixer-toggle '())
(defun my/amixer-toggle-mute (&optional unmute)
  "Toggle amizer muter state. With prefix UNMUTE, always unmute."
  (interactive"P")
  (when unmute (setq my/amixer-toggle nil))
  (funcall
   (or my/amixer-toggle
       (setq my/amixer-toggle
             (let ((muted nil))
               (lambda ()
                 (start-process-shell-command
                  "*amixer*"
                  (get-buffer-create "*amixer*")
                  (format "amixer -D pulse sset Master %s"
                          (if (setq muted (not muted))
                              "mute"
                            "unmute")))
                 (message (if muted "muted" "unmuted"))))))))

(defmacro my/amixer-define-change-vol
    (name perc up)
  (declare (indent 'defun))
  `(defun ,(symb 'my/amixer-vol- name) ()
     (interactive)
     (my/amixer-change-volume ,perc ,up)))

(my/amixer-define-change-vol up 3 t)
(my/amixer-define-change-vol up-large 7 t)
(my/amixer-define-change-vol down 3 nil)
(my/amixer-define-change-vol down-large 3 nil)


(spacemacs|define-transient-state sound-mixer
  :title "System Sounds Transient State"
  :doc "
 Commands^^^^^^
----------------------------
 [_j_/_k_] volume down / up
 [_J_/_K_] VOLUME down / up
 [_u_] toggle mute
 [_q_] quit "
  :bindings
  ("j" my/amixer-vol-down)
  ("J" my/amixer-vol-down-large)
  ("k" my/amixer-vol-up)
  ("K" my/amixer-vol-up-large)
  ("u" my/amixer-toggle)
  ("q" nil :exit t))
