;;; dired-fdclone-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dired-fdclone" "dired-fdclone.el" (22348 28214
;;;;;;  598609 916000))
;;; Generated autoloads from dired-fdclone.el

(autoload 'diredfd-enable-auto-revert "dired-fdclone" "\
Enable auto-revert settings for dired.

`dired-async' is supported.

\(fn)" nil nil)

(autoload 'diredfd-nav-mode "dired-fdclone" "\
Toggle nav mode.

\(fn &optional ARG)" t nil)

(autoload 'diredfd-find-file "dired-fdclone" "\
Visit the current file or directory.

\(fn)" nil nil)

(autoload 'diredfd-goto-top "dired-fdclone" "\
Go to the top line of the current file list.

\(fn)" t nil)

(autoload 'diredfd-goto-bottom "dired-fdclone" "\
Go to the bottom line of the current file list.

\(fn)" t nil)

(autoload 'diredfd-toggle-mark-here "dired-fdclone" "\
Toggle the mark on the current line.

\(fn)" t nil)

(autoload 'diredfd-toggle-mark "dired-fdclone" "\
Toggle the mark on the current line and move to the next line.
Repeat ARG times if given.

\(fn &optional ARG)" t nil)

(autoload 'diredfd-toggle-all-marks "dired-fdclone" "\
Toggle all marks.

\(fn)" t nil)

(autoload 'diredfd-mark-or-unmark-all "dired-fdclone" "\
Unmark all files if there is any file marked, or mark all non-directory files otherwise.
If ARG is given, mark all files including directories.

\(fn &optional ARG)" t nil)

(autoload 'diredfd-narrow-to-marked-files "dired-fdclone" "\
Kill all unmarked lines using `dired-kill-line'.

\(fn)" t nil)

(autoload 'diredfd-narrow-to-files-regexp "dired-fdclone" "\
Kill all lines except those matching REGEXP using `dired-kill-line'.

\(fn REGEXP)" t nil)

(autoload 'diredfd-goto-filename "dired-fdclone" "\
Jump to FILENAME.

\(fn FILENAME)" t nil)

(autoload 'diredfd-do-shell-command "dired-fdclone" "\
Open an ANSI terminal and run a COMMAND in it.

In COMMAND, the % sign is a meta-character and the following
macros are available.  All path names expanded will be escaped
with `shell-quote-argument'.

%P  -- Expands to the current directory name in full path.
%C  -- Expands to the name of the file at point.
%T  -- Expands to the names of the marked files, separated by
       spaces.
%M  -- Expands to the name of each marked file, repeating the
       command once for every marked file.
%X  -- Expands to the name of the file at point without the last
       suffix. (cf. `file-name-sans-extension')
%XM -- Expands to the name of each marked file without the last
       suffix, repeating the command once for every marked file.
%XT -- Expands to the names of the marked files without their
       last suffix, separated by spaces.
%%  -- Expands to a literal %.

\(fn COMMAND)" t nil)

(autoload 'diredfd-do-flagged-delete-or-execute "dired-fdclone" "\
Run `dired-do-flagged-delete' if any file is flagged for deletion.
If none is, run a shell command with all marked (or next ARG) files or the current file.

For a list of macros usable in a shell command line, see `diredfd-do-shell-command'.

\(fn &optional ARG)" t nil)

(autoload 'diredfd-enter "dired-fdclone" "\
Visit the current file, or enter if it is a directory.

\(fn)" t nil)

(autoload 'diredfd-enter-directory "dired-fdclone" "\
Enter DIRECTORY and jump to FILENAME.

\(fn &optional DIRECTORY FILENAME)" t nil)

(autoload 'diredfd-enter-parent-directory "dired-fdclone" "\
Enter the parent directory.

\(fn)" t nil)

(autoload 'diredfd-enter-root-directory "dired-fdclone" "\
Enter the root directory.

\(fn)" t nil)

(autoload 'diredfd-view-file "dired-fdclone" "\
Visit the current file in view mode.

\(fn)" t nil)

(autoload 'diredfd-do-pack "dired-fdclone" "\
Pack all marked (or next ARG) files, or the current file into an archive.

\(fn &optional ARG)" t nil)

(autoload 'diredfd-pack "dired-fdclone" "\
Pack FILES into ARCHIVE, asynchronously if ASYNC is non-nil.

\(fn FILES ARCHIVE &optional ASYNC)" nil nil)

(autoload 'diredfd-do-unpack "dired-fdclone" "\
Unpack all marked (or next ARG) files or the current file.

\(fn &optional ARG)" t nil)

(autoload 'diredfd-help "dired-fdclone" "\
Show the help window.

\(fn)" t nil)

(autoload 'dired-fdclone "dired-fdclone" "\
Enable FDclone mimicking settings for dired.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dired-fdclone-autoloads.el ends here
