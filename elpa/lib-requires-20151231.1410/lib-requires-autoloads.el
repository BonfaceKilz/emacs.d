;;; lib-requires-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lib-requires" "lib-requires.el" (22592 24777
;;;;;;  8541 425000))
;;; Generated autoloads from lib-requires.el

(let ((loads (get 'Library-Dependencies 'custom-loads))) (if (member '"lib-requires" loads) nil (put 'Library-Dependencies 'custom-loads (cons '"lib-requires" loads))))

(defvar libreq-file-header ";; Features that might be required by this library:\n;;\n" "\
*Header inserted by `libreq-insert-lib-requires-as-comment'.")

(custom-autoload 'libreq-file-header "lib-requires" t)

(autoload 'libreq-requires-tree "lib-requires" "\
The features `require'd by LIBRARY, as a tree.
The tree structure shows library dependencies: Each feature is
represented by its name or by a list of its name followed by the
features that it explicitly requires.

Argument LIBRARY is an Emacs-Lisp file name, or file name sans
extension.  This command loads LIBRARY before determining its
dependencies.  This means that LIBRARY must contain (provide LIBRARY).
If it does not, an error is raised.

Function `libreq-requires-tree' calls itself recursively on its
dependencies, so an attempt is made to load all of them.

Note: If a byte-compiled (`*.elc') version of a library is
available, it is loaded, in preference to the source library -
this is the standard behavior of `load-library'.  This means that
the tree of required features reflects the dependencies indicated
in the byte-compiled file, not the source file.  If the
byte-compiled file is out-of-date, so will be the result of
`libreq-requires-tree'.

A required feature that was loaded successfully is represented by a
  string that names the required feature.
A required file or feature that failed to load is represented by a
  symbol that names the required feature.

For example: Suppose that library `doremi.el' requires `ring+' and
`mwheel', and library `ring+' requires `ring'.  If `ring+' is
successfully loaded and `mwheel.el' is not, then the result is this:

  (mwheel (\"ring+\" (\"ring\")))

Argument CUMUL is used only for recursive calls, to accumulate the
required features.

See also command `libreq-requires-list'.

Note that `libreq-requires-tree' and `libreq-requires-list' are
roughly the opposite of `file-dependents' in library `loadhist'.

\(fn LIBRARY &optional CUMUL)" t nil)

(autoload 'libreq-requires-list "lib-requires" "\
The libraries ultimately `require'd by LIBRARY, as a flat list.
Each library (file or feature) is represented only once, and the list
is sorted.

A library is represented as for `libreq-requires-tree': a file-name
string for a successfully loaded required library, a feature-name
symbol for an unsuccessfully loaded required feature.

LIBRARY must contain (provide LIBRARY); otherwise, an error is raised.

Note that `libreq-requires-tree' and `libreq-requires-list' are
essentially the opposite of `file-dependents' in library `loadhist'.

\(fn LIBRARY)" t nil)

(autoload 'libreq-insert-lib-requires-as-comment "lib-requires" "\
Insert a comment listing all libraries ultimately required by LIBRARY.
See also `libreq-requires-list' and `libreq-requires-tree'.

\(fn LIBRARY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lib-requires-autoloads.el ends here
