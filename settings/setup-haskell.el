(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "which-function-mode"
  '(add-to-list 'which-func-modes 'haskell-mode))

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

(custom-set-variables
 '(haskell-stylish-on-save t))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type 'ghci))

(define-key haskell-mode-map (kbd "C-c i") 'haskell-navigate-imports)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

;; (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
;; (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
(provide 'setup-haskell)
