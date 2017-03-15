;;; org-doing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-doing" "org-doing.el" (22592 24964 512669
;;;;;;  720000))
;;; Generated autoloads from org-doing.el

(autoload 'org-doing-log "org-doing" "\
Logs the `description' of what you're doing now in the file
`org-doing-file' at the *top* of the file.

When `later-p' is true, logs the item as something to be done
later.

\(fn DESCRIPTION &optional LATER-P)" t nil)

(autoload 'org-doing "org-doing" "\
Interactive function for running any org-doing command.

The first part of the `command' string is parsed as a command:
- now: calls `org-doing-log'
- later: calls `org-doing-log'
- done: calls `org-doing-done'
- next: used alone, calls `org-doing-start-next'
        otherwise calls `org-doing-log'

If no match is found, `org-doing-log' is called and passed the entire
command string.

\(fn COMMAND)" t nil)

;;;***

;;;### (autoloads nil nil ("org-doing-pkg.el") (22592 24964 466004
;;;;;;  202000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-doing-autoloads.el ends here
