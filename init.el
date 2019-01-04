(package-initialize)

(setq user-full-name "Bonface M. K.")
(setq user-mail-address "bonfacemunyoki@gmail.com")
(defvar user-home-directory (concat (expand-file-name "~") "/"))

;; Set up a settings dir
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Set up vendor dir
(defvar bonface/vendor-dir 
      (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path bonface/vendor-dir)

(dolist (project (directory-files bonface/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'setup-package)
(require 'setup-envs)
(require 'setup-init)
(require 'setup-default-packages)
(require 'setup-eshell)
(require 'setup-perspective)
(require 'setup-paredit)
(require 'my-misc)
(require 'appearance)
(require 'setup-avy)
(require 'setup-switch-window)
(require 'setup-exwm)
(require 'setup-haskell)
(require 'sane-defaults)
(require 'setup-neotree)
(require 'setup-hledger)
(require 'all-the-icons)
(require 'mode-mappings)
(require 'setup-hugo)
(require 'setup-helm)
(require 'setup-yasnippet)
(require 'eproject)
(require 'buffer-move)
(require 'autopair)

(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'php-mode '(require 'php-ext))
(eval-after-load 'magit '(require
                          'setup-magit))
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown))
