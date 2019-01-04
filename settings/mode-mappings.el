;; shell-script-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; conf-mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; web-mode
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)


(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))


;; Turn off linum mode in org mode and web mode
;; linum-mode slows down Emacs when there is a large number
;; of lines.
(defun nolinum ()
  (global-linum-mode 0))

(add-hook 'org-mode-hook 'nolinum)
(add-hook 'web-mode-hook 'nolinum)

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; haskell
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-paths `(,(expand-file-name "markdown.css" bonface/vendor-dir)))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-hook 'php-mode-hook 'php-refactor-mode)
(defun my-php-mode-hook ()
  "My PHP-mode hook."
  (require-package 'flycheck-phpstan)
  (flycheck-mode t)
  (flycheck-select-checker 'phpstan))
(add-hook 'php-mode-hook 'my-php-mode-hook)
(require-package 'php-auto-yasnippets)

;; Javascript
(require-package 'js2-mode)
(require-package 'xref-js2)
(require-package 'tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook
          (lambda ()
            (tern-mode t)
            (js2-refactor-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)     (tern-ac-setup)))

;; Python
(require-package 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq pythoh-shell-interpreter "python"
      python-shell-interpreter-args "-i")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(setq shell-file-name "/bin/bash")
(elpy-enable)

;; Django
;; (require 'django-html-mode)
(require-package 'django-mode)
;; (add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;; Info mode
(add-to-list 'auto-mode-alist '("\\info.gz$" . info-mode))

(require-package 'hledger-mode)
(add-to-list 'auto-mode-alist '("\\.journal$" . hledger-mode))
(setq hledger-jfile "/home/bonface/self/finances/hledger.journal")

;; Enabling reading epub files
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(provide 'mode-mappings)

;; Org-mode
(require-package 'org-plus-contrib)
(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)
  (warn "toc-org not found"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
