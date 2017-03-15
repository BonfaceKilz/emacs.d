;;; org-board-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-board" "org-board.el" (22592 25002 164962
;;;;;;  810000))
;;; Generated autoloads from org-board.el

(autoload 'org-board-archive "org-board" "\
Archive the URL given by the current entry's :URL: property.

The attachment directory and the unique ID are created if not
already present.  See the docstring of `org-attach-dir'.

Every snapshot is stored in its own timestamped folder, and is
added as a link in the :ARCHIVED_AT: property.

\(fn)" t nil)

(autoload 'org-board-archive-dry-run "org-board" "\
Print the `wget' invocation that will be run, taking into
account the current options.  Creates an `org-attach' directory
and property if not already present.

\(fn)" t nil)

(autoload 'org-board-expand-regexp-alist "org-board" "\
With point in an org-board entry, add to the WGET_OPTIONS
according to `org-board-domain-regexp-alist'.

\(fn)" nil nil)

(autoload 'org-board-options-handler "org-board" "\
Expand WGET_OPTIONS according to `org-board-agent-header-alist'.

\(fn WGET-OPTIONS)" nil nil)

(autoload 'org-board-delete-all "org-board" "\
Delete all archives for the entry at point.

The parent attachment directory is not removed.  Note that all
attachments to the entry are deleted.

\(fn)" t nil)

(autoload 'org-board-open "org-board" "\
Open the archived version of the page pointed to by the URL property.
With prefix argument, temporarily flip the value of
`org-board-default-browser' and open there instead.

If that does not work, open a list of HTML files from the
most recent archive, in Dired.

\(fn ARG)" t nil)

(autoload 'org-board-open-with "org-board" "\
Open visited file in default external program, return exit code.

\(fn FILENAME-STRING ARG)" nil nil)

(autoload 'org-board-extend-default-path "org-board" "\
Extend a filename to end in `/index.html'.

Examples: `aurox.ch'  => `aurox.ch/index.html'
          `aurox.ch/' => `aurox.ch/index.html'.

\(fn FILENAME-STRING)" nil nil)

(autoload 'org-board-new "org-board" "\
Ask for a URL, create a property with it, and archive it.

\(fn URL)" t nil)

(autoload 'org-board-diff "org-board" "\
Recursively diff two archives from the same entry.

\(fn ARCHIVE1 ARCHIVE2)" t nil)

(autoload 'org-board-cancel "org-board" "\
Cancel the current org-board archival process.  Leave the
output buffer intact.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-board-autoloads.el ends here
