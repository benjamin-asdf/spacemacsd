;; -*- lexical-binding: t; -*-

(defface cos/comp-face-base
  '((t :bold t :height 1.2))
  "Face inherited by cos comp faces."
  :group 'cos-faces)

(cl-defstruct cos/comp-type
  class-string
  (in-gtags-ref-p nil :documentation "A function invoked with a single argument, a csharp class name. Returns non nil,
if the class name is known to inherit from this comp type. E.g. (it-gtags-ref-p LoreDoShowLocal) returns non nil when this struct refers to PrimaryEntityIndex.")
  comp-checker-function
  comp-face)

(defvar cos/comp-types '())
;; (setq cos/comp-types nil)
(defvar cos/comp-class-string-alist '()
  "Alist mapping all `cos/comp-types' with their class name in code.")


(defmacro cos/define-comp-type (name class-string color)
  "Define a component type with NAME.
Define a variable called cos/NAME-comp and set an instance of
`cos/comp-type'."
  ;; defface
  (let ((sym (symb 'cos/ name '-comp)))
    `(let* ((gtags-lut (make-hash-table :size 1024 :test #'equal))
            (in-gtags-lut-p
             (lambda (string)
               (unless (hash-table-keys gtags-lut)
                 (team/with-default-dir
                  idlegame-project-root
                  (with-temp-buffer
                    (process-file-shell-command
                     (format "global --result=grep --other --reference \"%s\" | rg \"public.*class\"" ,class-string)
                     nil t nil)
                    (->gg)
                    (while (re-search-forward "class \\(\\w+\\)" nil t)
                      (let ((s (match-string-no-properties 1)))
                        (unless (gethash s gtags-lut)
                          (puthash s t gtags-lut)))))))
               (gethash string gtags-lut))))
       (setq
        cos/comp-types
        (cons (defvar ,sym
                (make-cos/comp-type
                 :class-string ,class-string
                 :in-gtags-ref-p in-gtags-lut-p
                 :comp-checker-function
                 (defun ,(symb sym '-font-function) (lim)
                   (and
                    (re-search-forward "\\(Matcher\\)\.\\(\\w+\\)<\\(\\w+\\)>" nil t)
                    (let ((value (list (match-beginning 3) (match-end 3))))
                      (when (funcall in-gtags-lut-p
                             (match-string-no-properties 3))
                        (set-match-data value)
                        t))))
                 :comp-face
                 (defface ,(symb sym '-face)
                   '((t (:inherit cos/comp-face-base :foreground ,color)))
                   "")))
              cos/comp-types)))))


(defun cos/comp-assoc-class-string (string)
  "Initialize `cos/comp-class-string-alist', if it doesn't exsit.
Assoc STRING witht the corresponding `cos/comp-type'."
  (unless  cos/comp-class-string-alist
    (setq cos/comp-class-string-alist
          (--map
           `(,(cos/comp-type-class-string (symbol-value it)) ,(symbol-value it))
           cos/comp-types)))
  (assoc-default string cos/comp-class-string-alist))


(defun cos/comp-font-lock-keywords ()
  (--map
   `(,(cos/comp-type-comp-checker-function
       (symbol-value it)) .
       ,(cos/comp-type-comp-face (symbol-value it)))
   cos/comp-types))

(defun cos/add-font-lock-keywords ()
  (require 'treemacs-faces)
  (font-lock-add-keywords
   nil
   `(("AddReactEach" . 'treemacs-root-face))
   'end)
  (dolist (elm cos/comp-types)
    (font-lock-add-keywords
     nil
     `((,(cos/comp-type-comp-checker-function
          (symbol-value elm)) .
          ,(symbol-value (cos/comp-type-comp-face (symbol-value elm))))))))

(add-hook
 'csharp-mode-hook
 #'cos/add-font-lock-keywords)



;; (add-hook
;;  'csharp-mode-hook
;;  #'(lambda ()
;;      (font-lock-add-keywords
;;       nil
;;       '(("Contexts" . 'helm-ls-git-added-copied-face))
;;       'end)))

;; (setq csharp-mode-hook (cdr csharp-mode-hook))
;; (setq csharp-mode-hook nil)


;; now we only need to define our comps
;; adjust the helm source to use the alist func

(cos/define-comp-type
 value-comp
 "Component"
 "#FFD866")
(cos/define-comp-type
 primary-index-comp
 "PrimaryIndexComponent"
 "Tomato")
(cos/define-comp-type
 index-comp
 "IndexComponent"
 "Orange")
(cos/define-comp-type
 unique-value-comp
 "UniqueComponent"
 "#dd6bec")
(cos/define-comp-type
 unique-flag-comp
 "UniqueFlagComponent"
 "#83da7e")
(cos/define-comp-type
 flag-comp
 "FlagComponent"
 "#84ebf3")
