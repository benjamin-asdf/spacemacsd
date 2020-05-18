(defconst benj-csharp-scratches-dir "~/repos/csharp/csharp-scratches")

;; TODO get rid of, use yasnippet
(defun benj--csharp-program-snippet (name &optional contents)
  "Get snippet to add into csharp sample."
  (format "
using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections;
using System.Diagnostics;


public static class Programm {

    public static void Main(string[] args) {
        Console.WriteLine(\"==== %s ====\\n\");

        %s
    }
}
" name (or contents "")))

(defun benj-csharp-exclude-buffer ()
  "Exclude the whole buffer from compilation.
This uses `benj-charp-dont-compile-region' internally"
  (interactive)
  (benj-csharp-dont-compile-region (point-min) (point-max)))

(defun benj-csharp-dont-compile-region (beginning end)
  "Wrap region with 'if false' directive.
BEGINNING and END are numbers of the region BEGINNING and END."
  (setq b (make-marker))
  (setq e (make-marker))
  (set-marker b beginning (current-buffer))
  (set-marker e end (current-buffer))
  (save-excursion
    (goto-char (marker-position b))
    (insert "# if false\n")
    (goto-char (marker-position e))
    (insert "# endif")))

(defun benj-csharp-exclude-region ()
  "Eclude active region from compilation.
See `benj-csharp-dont-compile-region'"
  (interactive)
  (benj-csharp-dont-compile-region (region-beginning) (region-end)))


;; TODO figure out how to put a yasnippet there
(defun benj-create-new-csharp-scratch (&rest args)
  "Create a new skeleton csharp script. In `benj-csharp-scratches-dir'
args are key value pairs, meaningful:
:name - the name of snippet, ask the user if ommitted,
:contents - the intial contents, default is none "
  (interactive)
  (let ((name
         (or (plist-get args :name)
             (read-from-minibuffer "Name for csharp scrath: ")))
        (contents (or (plist-get args :contents) "")))


    (benj-csharp-exclude-all-files-from-project)
    (find-file (concat (file-name-as-directory benj-csharp-scratches-dir)
                       (format "%s.cs" (capitalize name))))
    (insert (benj--csharp-program-snippet name contents))
    (goto-char (point-min))
    (forward-line 5)))

(defun benj-csharp-exclude-all-files-from-project (&optional project)
  "Exclude all files in compilation for PROJECT. Defaults to sample project."
  (interactive)
  (dolist (file (directory-files (or project benj-csharp-scratches-dir) t ".*\.cs$"))
    (benj-csharp-exclude-file-at file)))


(defun benj-csharp-exclude-file-at (&optional path)
  "Add #if false around the the whole file at PATH, if not present already."
  (interactive "fFile to exclude from compilation:")
  (with-temp-file path
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (unless (looking-at-p "# if false")
      (benj-csharp-exclude-buffer))))



(defun benj-csharp-scratches-create-on-region ()
  "Create new csharp scratch with active region as initial content."
  (interactive)
  (benj-create-new-csharp-scratch :contents (buffer-substring-no-properties (region-beginning) (region-end))))


(defun benj-csharp-encapsulate-field ()
  "Put backing field syntax here."
  (interactive)
  (let ((type-name)
        (field-name)
        (backing-field-name))
    (evil-backward-WORD-begin 2)
    (re-search-forward "\\(\\(\\w\\|<\\|>\\)+\\)\s+\\(\\w+\\)" (point-at-eol) t)
    (setq type-name (match-string 1))
    (setq field-name (match-string 3))
    (setq backing-field-name (concat "_" field-name))
    (replace-match (format "%s %s" type-name backing-field-name))
    (forward-line 1)
    (open-line 1)
    (insert (format "public %s %s => %s ?? (%s = );" type-name field-name backing-field-name backing-field-name))
    (indent-according-to-mode)
    (search-backward "=")
    (evil-insert-state)
    (forward-char 2)))
