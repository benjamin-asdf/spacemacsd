;; -*- lexical-binding: t; -*-

(require 'team-utils "~/.spacemacs.d/layers/team-utils/funcs.el")

(defface cos/comp-face-base
  '((t :bold nil :height 1.1))
  "Face inherited by cos comp faces."
  :group 'cos-faces)

(cl-defstruct cos/comp-type
  class-string
  (in-gtags-ref-p nil :documentation "A function invoked with a single argument, a csharp class name. Returns non nil,
if the class name is known to inherit from this comp type. E.g. (it-gtags-ref-p LoreDoShowLocal) returns non nil when this struct refers to PrimaryEntityIndex.")
  comp-checker-function
  comp-face)

(defvar cos/comp-types '())
(defvar cos/comp-class-string-alist '()
  "Alist mapping all `cos/comp-types' with their class name in code.")


(defmacro cos/define-comp-type (name class-string color)
  "Define a component type with NAME.
Define a variable called cos/NAME-comp and set an instance of
`cos/comp-type'."
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
                    (re-search-forward "<\\(\\w+\\)>" nil t)
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



(defun cos/comp-face (s)
  "If S is a known Component reference,
return the appropriate comp-type-face, defined by `cos/comp-types'"
  (--first-result
   (let ((elm (symbol-value it)))
     (when (funcall
            (cos/comp-type-in-gtags-ref-p elm)
            s)
       (cos/comp-type-comp-face elm)))
   cos/comp-types))

(defun cos/comp-assoc-class-string (string)
  "Initialize `cos/comp-class-string-alist', if it doesn't exsit.
Assoc STRING witht the corresponding `cos/comp-type'."
  (unless  cos/comp-class-string-alist
    (setq cos/comp-class-string-alist
          (--map
           `(,(cos/comp-type-class-string (symbol-value it)) ,it)
           cos/comp-types)))
  (symbol-value (car (assoc-default string cos/comp-class-string-alist))))



(defconst cos/comp-ref-reg "\\([[:blank:]]*\\(\\w+\\)[[:blank:]]*,?\\)?")

(defface cos/composit-mem-face
  '((t . (:inherit font-lock-string-face :height 1.2)))
  "Face used for cos composit game system member functions")

(defvar cos/composit-mem-reg nil)
(defun cos/composit-mem-reg ()
  (unless cos/composit-mem-reg
    (setq cos/composit-mem-reg
          (team/with-file
           (concat idlegame-assets-dir "#/Sources/Helper/Entitas/BaseCompositeSystem.cs")
           (let ((list))
             (while (re-search-forward "public \\w+ \\(Add\\w+\\)(" (mark) t)
               (push (match-string-no-properties 1) list))
             (regexp-opt list)))))
  cos/composit-mem-reg)

(defun cos/add-font-lock-keywords ()
  (font-lock-add-keywords
   nil
   `((,(cos/composit-mem-reg) . 'cos/composit-mem-face)
     (,(concat "<" (string-times 3 cos/comp-ref-reg) ">")
      (2 (cos/comp-face (match-string-no-properties 2)) t t)
      (4 (cos/comp-face (match-string-no-properties 4)) t t)
      (6 (cos/comp-face (match-string-no-properties 6)) t t))
     )
   'end))



(cos/define-comp-type
 value-comp
 "Component"
 "Magenta3")
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
 "Chartreuse")
(cos/define-comp-type
 flag-comp
 "FlagComponent"
 "#84ebf3")



(add-hook 'csharp-mode-hook #'cos/add-font-lock-keywords)


(provide 'cos-comp-types)
