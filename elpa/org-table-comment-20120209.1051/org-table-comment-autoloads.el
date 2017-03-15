;;; org-table-comment-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-table-comment" "org-table-comment.el"
;;;;;;  (22592 24714 346888 961000))
;;; Generated autoloads from org-table-comment.el

(defalias 'org-table-comment-mode 'orgtbl-comment-mode)

(autoload 'orgtbl-comment-mode "org-table-comment" "\
Orgtbl comment mode.  Changes how orgtbl works for modes that don't support block comment regions (like emacs-lisp).

Currently supports radio tables through overlay interface.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-table-comment-autoloads.el ends here
