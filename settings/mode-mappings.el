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
(add-hook 'php-mode-hook 'fly-check-mode)
(require 'php-auto-yasnippets)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

;; Javascript
(require 'js2-mode)
(require 'xref-js2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; Python
(elpy-enable)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Django
(require 'django-html-mode)
(require 'django-mode)
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;; Info mode
(add-to-list 'auto-mode-alist '("\\info.gz$" . info-mode))

(require 'hledger-mode)
(add-to-list 'auto-mode-alist '("\\.journal$" . hledger-mode))

(provide 'mode-mappings)
