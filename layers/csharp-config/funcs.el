(defconst benj-chsarp-samples-dir "~/repos/csharp/csharp-samples")

;; TODO get rid of, use yasnippet
(defun benj--csharp-program-snippet (name &optional contents)
  "Get snippet to add into chsarp sample."
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


(add-hook 'csharp-mode-hook 'benj-charp-hook)

(defun benj-charp-hook()
  (benj-change-csharp-style)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (define-key evil-normal-state-map "gh" 'omnisharp-current-type-information)
  (setq-local buffer-file-coding-system 'windows-1256-unix)
  ;; windows only, slow
  (setq-local company-idle-delay 1.5)
  (smartparens-strict-mode -1))

;; hack for windows, because this is slow
(defun c-before-change (beg end))
(defun c-after-change (beg end old-len))



(defun benj-change-csharp-style()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(defun benj-dotnet-run()
  "Run dotnet run is current dir, open output in a dedicated buffer."
  (interactive)
  (benj-process-other-window "benj-dotnet-run-process" "*dotnet-run*" "dotnet" "run"))

(defun benj-csharp-exclude-file ()
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

(defun benj-chsarp-exclude-region ()
  "Eclude active region from compilation.
See `benj-csharp-dont-compile-region'"
  (interactive)
  (benj-csharp-dont-compile-region (region-beginning) (region-end)))


;; TODO figure out how to put a yasnippet there
(defun benj-create-new-chsarp-sample (&optional name)
  "Create a new skeleton chsarp script with NAME in the csharp samples dir."
  (interactive "sName for chsarp sample: ")
  (benj-chsarp-exclude-all-files-from-project)
  (find-file (concat (file-name-as-directory benj-chsarp-samples-dir)
                     (format "%s.cs" (capitalize name))))
  (insert (benj--csharp-program-snippet name))
  (goto-char (point-min))
  (forward-line 5))


(defun benj-chsarp-exclude-all-files-from-project (&optional project)
  "Exclude all files in compilation for PROJECT. Defaults to sample project."
  (interactive)
  (dolist (file (directory-files (or project benj-chsarp-samples-dir) t ".*\.cs$"))
    (benj-csharp-exclude-file-at file)))


(defun benj-csharp-exclude-file-at (&optional path)
  "Add #if false around the the whole file at PATH, if not present already."
  (interactive "fFile to exclude from compilation:")
  (with-temp-file path
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (unless (looking-at-p "# if false")
      (benj-csharp-exclude-file))))

(setq omnisharp-expected-server-version "1.34.15")

