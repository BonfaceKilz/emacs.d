;;; org-attach-screenshot-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-attach-screenshot" "org-attach-screenshot.el"
;;;;;;  (22592 25032 367323 357000))
;;; Generated autoloads from org-attach-screenshot.el

(autoload 'org-attach-screenshot "org-attach-screenshot" "\
Take an area screenshot and place it in the entry's attachment directory.

The user is interactively prompted for a base FILENAME for the
screenshot.  If the name is empty, a generic name will be
generated.  If the org entry has no defined attachment directory,
the user will be offered the choice to create one through the
`org-attach-screenshot-get-attach-dir' function.

The frame invoking the function gets hidden while taking the
screenshot unless a prefix argument PRFX is passed (this allows
taking screenshots of the Emacs session itself).  If no filename
extension is provided, .png will be added.

The command for invoking the external screenshot utility can be
customized using the `org-attach-screenshot-command-line' variable.

Note that the screenshots are not stored as actual attachments
which would mean that entries for the Attachments would be
written to the PROPERTIES section of a headline in addition to
the links being already placed inside the text.

\(fn PRFX FILENAME)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-attach-screenshot-autoloads.el ends here
