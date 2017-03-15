;;; org-gnome-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-gnome" "org-gnome.el" (22592 24920 660223
;;;;;;  973000))
;;; Generated autoloads from org-gnome.el

(defvar global-org-gnome-minor-mode nil "\
Non-nil if Global Org-Gnome minor mode is enabled.
See the `global-org-gnome-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-org-gnome-minor-mode'.")

(custom-autoload 'global-org-gnome-minor-mode "org-gnome" nil)

(autoload 'global-org-gnome-minor-mode "org-gnome" "\
Toggle Org-Gnome minor mode in all buffers.
With prefix ARG, enable Global Org-Gnome minor mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Org-Gnome minor mode is enabled in all buffers where
`org-gnome-turn-on' would do it.
See `org-gnome-minor-mode' for more information on Org-Gnome minor mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-gnome-autoloads.el ends here
