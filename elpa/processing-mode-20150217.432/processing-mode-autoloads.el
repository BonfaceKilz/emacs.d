;;; processing-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "processing-mode" "processing-mode.el" (22503
;;;;;;  35577 871373 796000))
;;; Generated autoloads from processing-mode.el

(autoload 'processing-find-sketch "processing-mode" "\
Find a processing sketch with NAME in `processing-sketchbook-dir'.
If ARG is non nil or `processing-sketchbook-dir' is nil create new
sketch in current directory.

\(fn NAME &optional ARG)" t nil)

(autoload 'processing-mode "processing-mode" "\
Major mode for Processing.
\\{java-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; processing-mode-autoloads.el ends here
