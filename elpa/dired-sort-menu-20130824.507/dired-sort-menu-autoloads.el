;;; dired-sort-menu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dired-sort-menu" "dired-sort-menu.el" (22348
;;;;;;  28107 25037 797000))
;;; Generated autoloads from dired-sort-menu.el

(let ((loads (get 'dired-sort-menu 'custom-loads))) (if (member '"dired-sort-menu" loads) nil (put 'dired-sort-menu 'custom-loads (cons '"dired-sort-menu" loads))))

(autoload 'dired-sort-menu-popup "dired-sort-menu" "\
Pop up and run \"Sort By\" menu for dired mode *in EVENT window*.

\(fn EVENT)" t nil)

(autoload 'dired-sort-menu-toggle-ignore-case "dired-sort-menu" "\
Toggle ls-lisp switch `ls-lisp-ignore-case' and update buffer.

\(fn)" t nil)

(autoload 'dired-sort-menu-toggle-dirs-first "dired-sort-menu" "\
Toggle ls-lisp switch `ls-lisp-dirs-first' and update buffer.

\(fn)" t nil)

(autoload 'dired-sort-menu-toggle-reverse "dired-sort-menu" "\
Toggle ls -r switch and update buffer.
Does not affect other sort switches.

\(fn)" t nil)

(autoload 'dired-sort-menu-toggle-recursive "dired-sort-menu" "\
Toggle ls -R switch and update buffer.
Does not affect other sort switches.

\(fn)" t nil)

(autoload 'dired-sort-menu-swap-config "dired-sort-menu" "\
Swap saved and current `dired' sort configuration.

\(fn)" t nil)

(autoload 'dired-sort-dialogue "dired-sort-menu" "\
A static dialogue version of the Dired sort menu.
This command *must* be run in the Dired buffer!

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("dired-sort-menu-pkg.el") (22348 28107
;;;;;;  78539 54000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dired-sort-menu-autoloads.el ends here
