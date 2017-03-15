;;; org-clock-csv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-clock-csv" "org-clock-csv.el" (22592 24986
;;;;;;  675416 758000))
;;; Generated autoloads from org-clock-csv.el

(autoload 'org-clock-csv "org-clock-csv" "\
Export clock entries from INFILE to CSV format.

When INFILE is a filename or list of filenames, export clock
entries from these files. Otherwise, use `org-agenda-files'.

See also `org-clock-csv-batch' for a function more appropriate
for use in batch mode.

\(fn &optional INFILE)" t nil)

(autoload 'org-clock-csv-batch "org-clock-csv" "\
Export clock entries from INFILE in CSV format to standard output.

This function is identical in function to `org-clock-csv' except
that it directs output to `standard-output'. It is intended for
use in batch mode.

\(fn &optional INFILE)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-clock-csv-autoloads.el ends here
