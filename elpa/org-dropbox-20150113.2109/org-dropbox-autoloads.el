;;; org-dropbox-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-dropbox" "org-dropbox.el" (22592 24951
;;;;;;  459641 941000))
;;; Generated autoloads from org-dropbox.el

(let ((loads (get 'org-dropbox 'custom-loads))) (if (member '"org-dropbox" loads) nil (put 'org-dropbox 'custom-loads (cons '"org-dropbox" loads))))

(defconst org-dropbox-version "1.2" "\
Version of the org-dropbox package.")

(autoload 'org-dropbox-version "org-dropbox" "\
Version of the org-dropbox package.

\(fn)" t nil)

(autoload 'org-dropbox-mode "org-dropbox" "\
Minor mode adding Dropbox notes to datetree.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-dropbox-autoloads.el ends here
