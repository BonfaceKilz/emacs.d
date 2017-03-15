;;; dired-toggle-sudo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dired-toggle-sudo" "dired-toggle-sudo.el"
;;;;;;  (22348 28094 239107 869000))
;;; Generated autoloads from dired-toggle-sudo.el

(autoload 'dired-toggle-sudo "dired-toggle-sudo" "\
Reopen current file or dired buffer with sudo.

If SUDO-USER is nil assume root.

If called with `universal-argument' (C-u), ask for username.

\(fn &optional SUDO-USER)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dired-toggle-sudo-autoloads.el ends here
