;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq xcb:debug-on t)
;;(setq debug-on-error t)

(setq user-full-name "Bonface M. K.")
(setq user-mail-address "bonfacemunyoki@gmail.com")

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Set up a settings dir
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up vendor dir
(defvar bonface/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path bonface/vendor-dir)

(dolist (project (directory-files bonface/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path "~/.emacs.d/elpa/xelb-0.15")
(add-to-list 'load-path "~/.emacs.d/elpa/exwm-0.19")

(require 'setup-init)
(require 'setup-envs)
(require 'setup-package)
(require 'setup-default-packages)
(require 'sane-defaults)
(require 'setup-eshell)
(require 'my-misc)
(require 'appearance)
;;(require 'setup-exwm)

;; TODO add deft
;; Golden ratio
(golden-ratio-mode 1)
;; disable interference from golden ratio
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; setup helm
(require 'setup-helm)

(require 'zoom-window)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Flyspell
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

(eval-after-load 'org '(require 'setup-org))
 
;; Map files to modes
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


