;; Setup default packages

(defvar bonface/packages '(ac-slime
                           all-the-icons
                           auto-complete
                           autopair
                           avy
                           better-defaults
                           buffer-move
                           change-inner
                           dash
                           desktop-environment
			   diminish
			   diff-hl
                           elpy
                           epresent
                           eproject
                           ess
                           eshell-autojump
                           expand-region
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
			   haskell-mode
			   helm-exwm
                           helm-projectile
			   highlight-escape-sequences
			   highlight-symbol
                           htmlize
                           magit
                           magit-org-todos
                           markdown-mode
                           marmalade
			   move-text
                           multiple-cursors
                           neotree
			   nodejs-repl
                           org
                           paredit
			   php-mode
                           php-refactor-mode
                           powerline
			   prodigy
			   persp-projectile
			   projectile
                           py-autopep8
                           rvm
			   s
                           smart-forward
                           smartparens
			   string-edit
			   switch-window
                           use-package
                           undo-tree
			   visual-regexp
                           web-mode
                           writegood-mode
                           yaml-mode
			   yasnippet
			   yasnippet-snippets
                           zoom-window)
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
