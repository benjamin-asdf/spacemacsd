
(require 'team-utils)

(defun prefab-p (s)
  (s-ends-with-p ".prefab" s))
(defun meta-p (s)
  (s-ends-with-p ".meta" s))

(team/def-memoized asset--usages (guid)
  "Return a list of file names where GUID is used."
  (team/with-default-dir
   idlegame-assets-dir
   (condition-case nil
       (apply
        #'process-lines
        `("rg"
          "-IlN"
          ,(format "guid: %s" guid)))
     (progn
       (message "%s appears to be unused" guid)
       nil))))

(team/def-memoized asset-usages (file)
  "Return a list of file names where FILE is used."
  (team/with-default-dir
   idlegame-assets-dir
   (--remove (s-ends-with-p ".meta" it)
             (asset--usages
              (team-unity/file-guid
               (s-chop-prefix
                "Assets/"
                file))))))

(defun team-unity/asset-path-from-guid (guid)
  (--first
   (s-ends-with-p ".meta" it)
   (asset--usages guid)))




(defconst yml-terminator "^--- ")

(cl-defstruct
    unity-asset-usage
  prefab-path
  script-guid
  script-path
  game-object-name
  field-name
  asset-type)

(defun team/search-in-syntax-bounds (terminator re)
  "Search RE around point, terminated by both sides with TERMINATOR.
Forward the return value of `re-search-forward'."
  (re-search-backward terminator)
  (forward-char 1)
  (re-search-forward
   re
   (save-excursion
     (re-search-forward terminator nil t))
   t))

(defun team-unity/search-this-yml-entry (re)
  (team/search-in-syntax-bounds yml-terminator re))

(team/def-memoized team-unity/detailed-asset-usage (file-or-meta)
  "Return a list `unity-asset-usage' for FILE-OR-META."
  (team/with-default-dir
   idlegame-assets-dir
   (--map
    (let ((items '()))
      (team/with-file
       it
       (while
           (re-search-forward
            (format
             (concat
              ;;  1
              ;; inside some arr
              "\\("
              "^  - {fileID: [0-9]+, guid: %1$s,"
              "\\)"
              ;;  2, 3
              ;; assigned to a field
              "\\|"
              "\\("
              "\\(\\w+\\): {fileId: [0-9]+, guid: %1$s"
              "\\)"
              ;;  4
              ;; as override
              "\\|"
              "\\("
              "^      objectReference: {fileId: [[:alnum:]]+, guid: %1$s,"
              "\\)")
             (team-unity/file-guid
              file-or-meta))
            nil t)
         (when (match-string 4)
           ;;  go upwarts to the transform modification the guid is the prefab guid
           (error "Encountered an override reference, this is not supported yet. Prefab: %s\n guid: %s\nwas checking %s" it (team-unity/file-guid file-or-meta) file-or-meta))
         (let ((field-name (match-string 3)))
           (save-excursion
             (unless
                 (team-unity/search-this-yml-entry
                "^  m_Script: {fileId: [[:alnum:]]+, guid: \\(\\w+\\), type: 3}")
               (error "Did not find a script guid inside game object.\n Prefab: %s\n guid: %s"
                        it
                        (team-unity/file-guid file-or-meta)))
             (let* ((script-guid (match-string 1))
                    (script-file-path (team-unity/asset-path-from-guid script-guid)))
               (re-search-backward "^GameObject:$")
               (unless
                   (team-unity/search-this-yml-entry
                    "^  m_Name: \\(.*\\)$")
                 (error "Did not find game object name, last pos %s:%s\nguid:%s\nfile-or-meta:%s" it (point) (team-unity/file-guid file-or-meta) file-or-meta))
               (push
                (make-unity-asset-usage
                 :prefab-path it
                 :script-guid script-guid
                 :script-path script-file-path
                 :game-object-name (match-string 1)
                 :field-name field-name
                 :asset-type (cos/asset-type it))
                items))))))
      items)
    (--filter
     (s-ends-with-p ".prefab" it)
     (asset-usages file-or-meta)))))


;;; can have a func that lets you select which prefab
(defun team-unity/field-ref-search (field-name &optional script-file)
  (team/with-default-dir
   idlegame-assets-dir
   (setq
    script-file
    (s-chop-prefix
     "Assets/"
     (or
      script-file
      (buffer-file-name))))
   (team/with-file
    (or
     (--first
      (s-ends-with-p ".prefab" it)
      (asset-usages script-file))
     (error
      "%s is not used on any prefab"
      script-file))
    (unless
        (re-search-forward
         (format
          "^  m_Script: {fileID: [[:alnum:]]+, guid: %s, type: 3}$"
          (team-unity/file-guid script-file)) nil t)
      ;;  might be a prefab override
      (error "Did not find script monobehaviour syntax for %s in %s"
             script-file (buffer-file-name)))
    (team-unity/search-this-yml-entry
     (format
      "^  %s: {fileId: [[:alnum:]]+, guid: \\(\\w+\\), type: [[:alnum:]]+}$"
      field-name))
    (file-name-nondirectory
     (-first #'meta-p
              (asset--usages (match-string 1)))))))

(provide 'unity-asset-usages)
