;; Setup default packages

(defvar bonface/packages '(ac-slime
                           all-the-icons
                           auto-complete
                           avy
                           better-defaults
			   diff-hl
                           elpy
                           epresent
                           ess
                           eshell-autojump
                           f
			   fill-column-indicator
                           feature-mode
                           flycheck
			   focus
                           gist
                           go-autocomplete
                           go-eldoc
                           go-mode
                           golden-ratio
			   guide-key
                           graphviz-dot-mode
                           haml-mode
			   helm-exwm
			   highlight-escape-sequences
			   highlight-symbol
                           htmlize
                           magit
                           markdown-mode
                           marmalade
			   move-text
                           neotree
			   nodejs-repl
                           org
                           paredit
			   php-mode
                           php-refactor-mode
                           powerline
			   prodigy
			   projectile
                           py-autopep8
                           rvm
                           smartparens
			   string-edit
			   switch-window
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
