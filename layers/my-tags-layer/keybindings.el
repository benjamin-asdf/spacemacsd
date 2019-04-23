(spacemacs/declare-prefix "od" "tags")

(spacemacs/set-leader-keys "odP" 'ggtags-visit-project-root)
(spacemacs/set-leader-keys "odh" 'ggtags-view-tag-history)
(spacemacs/set-leader-keys "odf" 'ggtags-find-file)
(spacemacs/set-leader-keys "odg" 'ggtags-grep)
(spacemacs/set-leader-keys "ods" 'ggtags-find-other-symbol)
(spacemacs/set-leader-keys "odr" 'ggtags-find-reference)
(spacemacs/set-leader-keys "od`" 'ggtags-save-to-register)

(spacemacs/set-leader-keys "odq" 'ggtags-query-replace)
(spacemacs/set-leader-keys "odn" 'ggtags-next-mark)
(spacemacs/set-leader-keys "odp" 'ggtags-prev-mark)
(spacemacs/set-leader-keys "odD" 'ggtags-show-definition)
(spacemacs/set-leader-keys "odd" 'ggtags-find-definition)
;;TODO make this xref thing work, at least figure out how to bind c-[ lul
