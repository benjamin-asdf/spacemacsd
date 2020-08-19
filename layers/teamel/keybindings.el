(team/spacemacs-define-keys
 "oi"
 "teamel"
 '("l" . teamel/curl-yank)
 '("b" . teamel-add-debug-button)
 '("B" . teamel-add-debug-button-with-region)
 '("S" . teamel/yank-idlegame-sln)
 '("s" . teamel/open-sources)
 '("k" . teamel/add-config-here)
 )


(spacemacs/set-leader-keys
  "a"  'flycheck-display-error-at-point)
