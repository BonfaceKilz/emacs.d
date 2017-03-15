;;; latex-extra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "latex-extra" "latex-extra.el" (22383 34860
;;;;;;  979116 990000))
;;; Generated autoloads from latex-extra.el

(autoload 'latex/setup-auto-fill "latex-extra" "\
Set the function used to fill a paragraph to `latex/auto-fill-function'.

\(fn)" t nil)

(autoload 'latex/setup-keybinds "latex-extra" "\
Obsolete function. Use (add-hook 'LaTeX-mode-hook #'latex-extra-mode) instead.

\(fn)" t nil)

(autoload 'latex-extra-mode "latex-extra" "\
Defines extra commands and keys for LaTeX-mode.

To activate just call
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode)

The additions of this package fall into the following three
categories:

1-Key Compilation
=================

Tired of hitting C-c C-c 4 times (latex, bibtex, latex, view) for
the document to compile? This defines a much needed command that does
*everything* at once, and even handles compilation errors!

  C-c C-a `latex/compile-commands-until-done'

Navigation
==========

Five new keybindings are defined for navigating between
sections/chapters. These are meant to be intuitive to people familiar
with `org-mode'.

  C-c C-n `latex/next-section'
    Goes forward to the next section-like command in the buffer (part,
    chapter, (sub)section, or (sub)paragraph, whichever comes first).
  C-c C-u `latex/up-section'
    Goes backward to the previous section-like command containing this
    one. For instance, if you're inside a subsection it goes up to the
    section that contains it.
  C-c C-f `latex/next-section-same-level'
    Like next-section, except it skips anything that's \"lower-level\" then
    the current one. For instance, if you're inside a subsection it finds
    the next subsection (or higher), skipping any subsubsections or
    paragraphs.
  C-M-f `latex/forward-environment'
    Skip over the next environment, or exit the current one, whichever
    comes first.
  C-M-e `latex/end-of-environment'
    Exit the current environment, and skip over some whitespace
    afterwards. (Like `LaTeX-find-matching-end', but a little more useful.)

  C-M-b `latex/backward-environment'
  C-M-a `latex/beginning-of-environment'
  C-c C-p `latex/previous-section'
  C-c C-b `latex/previous-section-same-level'
    Same as above, but go backward.

Whitespace Handling
===================

`latex-extra.el' improves `auto-fill-mode' so that it only applies to
text, not equations. To use this improvement, just activate
`auto-fill-mode' as usual.

It also defines a new command:

  C-c C-q `latex/clean-fill-indent-environment'
    Completely cleans up the entire current environment. This involves:

    1. Removing extraneous spaces and blank lines.
    2. Filling text (and only text, not equations).
    3. Indenting everything.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; latex-extra-autoloads.el ends here
