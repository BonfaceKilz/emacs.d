;; Setup default packages

(defvar bonface/packages '(ac-slime
                           auto-complete
                           avy
			   diff-hl
                           ess
                           f
			   fill-column-indicator
                           feature-mode
                           flycheck
                           gist
                           go-autocomplete
                           go-eldoc
                           go-mode
                           golden-ratio
			   guide-key
                           graphviz-dot-mode
                           haml-mode
			   highlight-escape-sequences
                           htmlize
                           magit
                           markdown-mode
                           marmalade
			   move-text
			   nodejs-repl
                           org
                           paredit
			   php-mode
                           php-refactor-mode
                           powerline
			   prodigy
			   projectile
                           rvm
                           smartparens
                           smex
			   string-edit
                           web-mode
                           undo-tree
			   visual-regexp
                           writegood-mode
                           yaml-mode
			   yasnippet
			   yasnippet-snippets)
  "Default packages")

(defun bonface/packages-installed-p ()
  (loop for pkg in bonface/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (bonface/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg bonface/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'setup-default-packages)
