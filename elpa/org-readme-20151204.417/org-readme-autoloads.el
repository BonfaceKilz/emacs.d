;;; org-readme-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-readme" "org-readme.el" (22592 24787 872384
;;;;;;  304000))
;;; Generated autoloads from org-readme.el

(autoload 'org-readme-convert-to-markdown "org-readme" "\
Convert Readme.org to markdown Readme.md.

\(fn)" t nil)

(autoload 'org-readme-convert-to-emacswiki "org-readme" "\
Converts Readme.org to oddmuse markup and uploads to emacswiki.

\(fn)" t nil)

(autoload 'org-readme-git "org-readme" "\
Add current file and other relevant files to git.
If ADDMELPA and/or ADDELGET are non-nil then add a melpa/el-get recipe,
and either of these arguments are filepaths then use those files as the
recipes.

\(fn ADDMELPA ADDELGET)" t nil)

(autoload 'org-readme-gen-info "org-readme" "\
With the proper tools, generate an info and dir from the current readme.org.

\(fn)" t nil)

(autoload 'org-readme-sync "org-readme" "\
Syncs Readme.org with current buffer.
When COMMENT-ADDED is non-nil, the comment has been added and the syncing should begin.

\(fn &optional COMMENT-ADDED)" t nil)

(autoload 'org-readme-to-commentary "org-readme" "\
Replace Commentary section in elisp file with text from Readme.org.

\(fn)" t nil)

(autoload 'org-readme-top-header-to-readme "org-readme" "\
Copy top header from the elisp file into the readme file as Library Information.
The top header is defined as all text between the start of the file and the first 
match to `org-readme-end-section-regexp'.

\(fn)" t nil)

(autoload 'org-readme-changelog-to-readme "org-readme" "\
This puts the Emacs Lisp change-log into the Readme.org file.

\(fn)" t nil)

(autoload 'org-readme-update-required-features-section "org-readme" "\
Update the required features section of the elisp file.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("org-readme-pkg.el") (22592 24788 2389
;;;;;;  923000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-readme-autoloads.el ends here
