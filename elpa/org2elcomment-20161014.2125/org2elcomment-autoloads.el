;;; org2elcomment-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org2elcomment" "org2elcomment.el" (22592 24651
;;;;;;  674829 584000))
;;; Generated autoloads from org2elcomment.el

(autoload 'org2elcomment-anywhere "org2elcomment" "\
Convert ORG-FILE to the commentary section in EL-FILE.
This command can be invoked anywhere inside Emacs.

\(fn EL-FILE &optional ORG-FILE)" nil nil)

(autoload 'org2elcomment "org2elcomment" "\
Conver the current org file to the commentary section in EL-FILE.
This command must be invoked when the current buffer is in `org-mode'.

\(fn EL-FILE)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org2elcomment-autoloads.el ends here
