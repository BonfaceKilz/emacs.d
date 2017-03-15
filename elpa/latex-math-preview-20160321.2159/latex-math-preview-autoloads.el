;;; latex-math-preview-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "latex-math-preview" "latex-math-preview.el"
;;;;;;  (22383 34731 733605 678000))
;;; Generated autoloads from latex-math-preview.el

(let ((loads (get 'latex-math-preview 'custom-loads))) (if (member '"latex-math-preview" loads) nil (put 'latex-math-preview 'custom-loads (cons '"latex-math-preview" loads))))

(defstruct latex-math-preview-symbol source display func args image math)

(autoload 'latex-math-preview-expression "latex-math-preview" "\
Preview a TeX maths expression at (or surrounding) point.
The `latex-math-preview-function' variable controls the viewing method.
The LaTeX notations which can be matched are $...$, $$...$$ or
the notations which are stored in `latex-math-preview-match-expression'.

\(fn)" t nil)

(autoload 'latex-math-preview-save-image-file "latex-math-preview" "\


\(fn USE-CUSTOM-CONVERSION &optional OUTPUT)" t nil)

(autoload 'latex-math-preview-insert-mathematical-symbol "latex-math-preview" "\
Insert LaTeX mathematical symbols with displaying.

\(fn &optional NUM)" t nil)

(autoload 'latex-math-preview-insert-text-symbol "latex-math-preview" "\
Insert symbols for text part with displaying.

\(fn &optional NUM)" t nil)

(autoload 'latex-math-preview-insert-symbol "latex-math-preview" "\
Insert LaTeX mathematical symbols with displaying.

\(fn &optional NUM)" t nil)

(autoload 'latex-math-preview-last-symbol-again "latex-math-preview" "\
Insert last symbol which is inserted by `latex-math-preview-insert-symbol'

\(fn)" t nil)

(autoload 'latex-math-preview-beamer-frame "latex-math-preview" "\
Display beamer frame at current position.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; latex-math-preview-autoloads.el ends here
