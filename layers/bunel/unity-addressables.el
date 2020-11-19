(defconst team-unity/load-group-file "Assets/AddressableAssetsData/AssetGroups/Default Local Group.asset")

(defun team-unity/true-file-base (file)
  (string-trim-right (file-name-base file) ".prefab"))


(defun fix-load-group (file)
  "Add FILE's guid to the load groups, or fix existing entry."
  (interactive"f")
  (cos/with-idlegame-proj-root
   (team/with-file
    team-unity/load-group-file
    (if (re-search-forward (team-unity/true-file-base file) nil t)
        (progn
          (forward-line -1)
          (skip-chars-forward "^:")
          (forward-char 1)
          (kill-line)
          (insert " ")
          (insert (team-unity/file-guid file)))
      (re-search-forward "  m_Settings:")
      (forward-line -2)
      (insert (format "  - m_GUID: %s
    m_Address: %s
    m_ReadOnly: 0
    m_SerializedLabels: []
    m_MainAsset: {fileID: 0}
    m_TargetAsset: {fileID: 0}
"
                      (team-unity/file-guid file)
                      (team-unity/true-file-base file)))))))





(defun team-unity/address-from-guid (guid)
  "Check load group file and return the sting address of GUID,
if successfull, nil otherwise."
  (team/check-file
   team-unity/load-group-file
   (re-search-forward
    (format "  - m_GUID: %s$" guid))
   (forward-line 1)
   (skip-chars-forward "^:")
   (forward-char 2)
   (buffer-substring (point) (point-at-eol))))




(provide 'unity-addressables)
