;; Setup default packages

(defvar bonface/packages '(ac-slime
                           auto-complete
                           avy
                           ess
                           f
                           feature-mode
                           flycheck
                           go-autocomplete
                           go-eldoc
                           go-mode
                           golden-ratio
                           graphviz-dot-mode
                           haml-mode
                           htmlize
                           magit
                           markdown-mode
                           marmalade
                           org
                           paredit
			   php-mode
                           php-refactor-mode
                           powerline
                           rvm
                           smartparens
                           smex
                           web-mode
                           undo-tree
                           ;;winner-mode
                           writegood-mode
                           yaml-mode
			   yasnippet)
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
