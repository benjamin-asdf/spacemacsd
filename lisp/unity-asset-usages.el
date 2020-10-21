
(require 'team-utils)

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
  (--remove (s-ends-with-p ".meta" it)
            (asset--usages (team-unity/file-guid file))))

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

(defun team-unity/detailed-asset-usage (file-or-meta)
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
           (error "Encountered an override reference, this is not supported yet"))
         (let ((field-name (match-string 3)))
           (save-excursion
             (re-search-backward
              yml-terminator)
             (forward-char 1)
             (unless
                 (re-search-forward
                  "^  m_Script: {fileId: [[:alnum:]]+, guid: \\(\\w+\\), type: 3}"
                  (save-excursion
                    (re-search-forward yml-terminator nil t))
                  t)
               (error "Did not find a script guid inside game object.\n Prefab: %s\n guid: %s"
                      it
                      (team-unity/file-guid file-or-meta)))
             (let* ((script-guid (match-string 1))
                    (script-file-path (team-unity/asset-path-from-guid script-guid)))
               (re-search-backward "^GameObject:$")
               (unless
                   (re-search-forward
                    "^  m_Name: \\(\\w+\\)$" (save-excursion
                                               (re-search-forward yml-terminator nil t)))
                 (error "Did not find game object name, last pos %s:%s" it (point)))
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



(provide 'unity-asset-usages)
