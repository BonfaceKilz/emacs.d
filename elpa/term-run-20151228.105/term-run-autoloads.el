;;; term-run-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "term-run" "term-run.el" (22349 35457 802864
;;;;;;  276000))
;;; Generated autoloads from term-run.el

(autoload 'term-run "term-run" "\
Run PROGRAM in BUFFER-OR-NAME with ARGS in terminal buffer.

If BUFFER-OR-NAME is given, use this buffer.  In this case, old process in the
buffer will be destroyed.  Otherwise, new buffer will be generated automatically
from PROGRAM.

This function returns the buffer where the process starts running.

\(fn PROGRAM &optional BUFFER-OR-NAME &rest ARGS)" nil nil)

(autoload 'term-run-shell-command "term-run" "\
Run COMMAND in terminal buffer.

If NEW-BUFFER-P is given or called with prefix argument, generate new terminal
buffer for running COMMAND.  Otherwise, use the same buffer.  In this case, old
process in the buffer will be destroyed.

This function returns the buffer where the process starts running.

\(fn COMMAND &optional NEW-BUFFER-P)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; term-run-autoloads.el ends here
