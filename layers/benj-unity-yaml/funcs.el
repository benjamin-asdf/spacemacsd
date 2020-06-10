(defconst benj-unity-yaml/prefab-insance-start "^--- !u!1001")

(defun benj-unity-yaml/jump-to-next-prefab-instance ()
  (interactive)
  (re-search-forward benj-unity-yaml/prefab-insance-start nil t))
