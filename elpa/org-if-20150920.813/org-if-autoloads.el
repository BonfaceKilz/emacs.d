;;; org-if-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ob-org-if" "ob-org-if.el" (22592 24906 377083
;;;;;;  433000))
;;; Generated autoloads from ob-org-if.el

(autoload 'org-babel-execute:org-if "ob-org-if" "\
Execute a block of ORG-IF code with org-babel.
This function is called by `org-babel-execute-src-block'

\(fn BODY PARAMS)" nil nil)

;;;***

;;;### (autoloads nil "org-if-active" "org-if-active.el" (22592 24906
;;;;;;  607080 762000))
;;; Generated autoloads from org-if-active.el

(defvar org-if-active-mode nil "\
Non-nil if Org-If-Active mode is enabled.
See the `org-if-active-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-if-active-mode'.")

(custom-autoload 'org-if-active-mode "org-if-active" nil)

(autoload 'org-if-active-mode "org-if-active" "\
This mode toggles whether the org-if system is active.

\(fn &optional ARG)" t nil)

(autoload 'activate-org-if "org-if-active" "\
Activate org-if-active minor-mode.
When NO-NAVIGATE-P is specified, do not go to file \"index.org\" in current directory.

\(fn &optional NO-NAVIGATE-P)" t nil)

(autoload 'deactivate-org-if "org-if-active" "\
Deactivate org-if-active minor-mode.

\(fn)" t nil)

(autoload 'toggle-org-if-active-mode "org-if-active" "\
Toggle `org-if-active-mode'.

\(fn)" t nil)

(autoload 'org-if-save-and-quit "org-if-active" "\
Save state of current org-if session in a file in `org-if-save-dir'.
Then quit.

\(fn)" t nil)

(autoload 'org-if-restore "org-if-active" "\
Restore state of `*org-if-current-env*' and `*org-if-current-file*' from save.
Also restore last visited file.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-if-mode" "org-if-mode.el" (22592 24906
;;;;;;  653746 886000))
;;; Generated autoloads from org-if-mode.el

(autoload 'org-if-mode "org-if-mode" "\
Major mode for ORG-IF programming language.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("org-if-interpreter.el" "org-if-link.el"
;;;;;;  "org-if-misc.el" "org-if-pkg.el" "org-if-reader.el" "org-if.el")
;;;;;;  (22592 24906 743745 842000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-if-autoloads.el ends here
