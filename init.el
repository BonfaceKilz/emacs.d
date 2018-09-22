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
(add-to-list 'load-path settings-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Set up vendor dir
(defvar bonface/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path bonface/vendor-dir)

(dolist (project (directory-files bonface/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(require 'setup-init)
(require 'setup-envs)
(require 'setup-package)
(require 'setup-default-packages)
(require 'sane-defaults)
(require 'setup-eshell)
(require 'setup-perspective)
(require 'setup-paredit)
(require 'my-misc)
(require 'appearance)
(require 'setup-avy)
;;(require 'setup-exwm)

(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'php-mode '(require 'php-ext))
(eval-after-load 'magit '(require 
                          'setup-magit))

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode
          scala-mode)
(add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
;; initial setups for specific modes
(require 'setup-helm)
(require 'setup-yasnippet)

(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; TODO add deft
;; Golden ratio
(golden-ratio-mode 1)
;; disable interference from golden ratio
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)


(require 'zoom-window)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; setting up multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Flyspell
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")


;; Map files to modes
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


